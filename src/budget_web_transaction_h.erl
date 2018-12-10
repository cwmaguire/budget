-module(budget_web_transaction_h).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).

%% Custom callbacks.
-export([budget_post/2]).
-export([fetch/2]).
-export([fix_dates/1]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, fetch}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, budget_post}],
		Req, State}.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            QsVals = cowboy_req:parse_qs(Req),
            MaybeTxKey = lists:keyfind(<<"tx">>, 1, QsVals),
            MaybeParentKey = lists:keyfind(<<"parent">>, 1, QsVals),
            case {MaybeTxKey, MaybeParentKey} of
                {{_, Tx}, _} ->
                    {tx_exists(b2i(Tx)), Req, State};
                {_, {_, Parent}} ->
                    {tx_exists(b2i(Parent)), Req, State};
                _ ->
                    {true, Req, State}
            end;
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req),
            KVs = kvs(Body),
            Tx = list_to_integer(proplists:get_value("tx_id", KVs)),
            {tx_exists(Tx), Req1, [{kvs, KVs} | State]};
        _ ->
            {true, Req, State}
    end.

tx_exists(TxId) ->
    Sql = "select id "
          "from transaction t "
          "where t.id = $1; ",
    TxId == b2i(budget_query:fetch_value(Sql, [TxId])).

budget_post(Req, State) ->
    KVs = proplists:get_value(kvs, State),
    Tx = list_to_integer(proplists:get_value("tx_id", KVs)),
    Callback = proplists:get_value("callback", KVs),
    Hash = copy_parent(Tx),
    %io:format("Returning ~p~n", [Hash]),
    URL = <<"/transaction?tx_id=",
            (integer_to_binary(Hash))/binary,
            "&callback=",
            (list_to_binary(Callback))/binary>>,
    {{true, URL}, Req, State}.

kvs(Binary) ->
    Bins = binary:split(Binary, [<<"&">>, <<"=">>], [global]),
    kvs(Bins, []).

kvs([KBin, VBin | Rest], KVs) ->
    K = binary_to_list(KBin),
    V = binary_to_list(VBin),
    kvs(Rest, [{K, V} | KVs]);
kvs(_, KVs) ->
    KVs.

%budget_get(Req, #{from_date := From, to_date := To}) ->
fetch(Req, State) ->
    QsVals = cowboy_req:parse_qs(Req),

    MaybeTxKey = lists:keyfind(<<"tx_id">>, 1, QsVals),

    MaybeParentKey = lists:keyfind(<<"parent">>, 1, QsVals),

    case {MaybeTxKey, MaybeParentKey} of
      {{<<"tx_id">>, _TxId}, _} ->
          fetch_transaction(QsVals, Req, State);
      {_, {<<"parent">>, _Parent}} ->
          fetch_children(QsVals, Req, State);
      _ ->
          fetch_transactions_by_date(QsVals, Req, State)
    end.

fetch_transaction(QsVals, Req, State) ->
    {_, Callback} = lists:keyfind(<<"callback">>, 1, QsVals),
    {_, TxId} = lists:keyfind(<<"tx_id">>, 1, QsVals),
    WhereClause = "where t.id = $1 ",
    Sql = tx_query(WhereClause),
    Params = [TxId],
    Script = budget_query:fetch_jsonp(Sql, Params, Callback),
    {Script, Req, State}.

fetch_transactions_by_date(QsVals, Req, State) ->
    {_, Callback} = lists:keyfind(<<"callback">>, 1, QsVals),
    {_, RawFrom} = lists:keyfind(<<"from">>, 1, QsVals),
    {_, RawTo} = lists:keyfind(<<"to">>, 1, QsVals),
    From = to_date(RawFrom),
    To = to_date(RawTo),

    WhereClause = "where t.date between $1 and $2 ",
    Sql = tx_query(WhereClause),
    Params = [From, To],
    Script = budget_query:fetch_jsonp(Sql,
                                      Params,
                                      Callback),
	{Script, Req, State}.

fetch_children(QsVals, Req, State) ->
    {_, Callback} = lists:keyfind(<<"callback">>, 1, QsVals),
    {_, Parent} = lists:keyfind(<<"parent">>, 1, QsVals),

    Sql = "select count(*) \"count\" "
          "from transaction "
          "where parent = $1;",
    Params = [Parent],
    Script = budget_query:fetch_jsonp(Sql,
                                      Params,
                                      Callback),
	{Script, Req, State}.

tx_query(WhereClause) ->
    GroupBy = "t.id, "
              "t.acct_type, "
              "t.acct_num, "
              "t.date, "
              "t.posted, "
              "t.cheq_num, "
              "t.desc_1, "
              "t.desc_2, "
              "t.cad, "
              "t.usd, "
              "t.parent, "
              "t.child_number ",

    "select t.*, "
    "       string_agg(c.name || '||' || tc.id, ', ') categories, "
    "       max(t_child.child_number) > 0 \"is_parent\" "
    "from transaction t "
    "left join transaction t_child "
    "  on t.id = t_child.parent "
    "left join transaction_category tc "
    "  on t.id = tc.tx_id "
    "left join category c "
    "  on tc.cat_id = c.id " ++
    WhereClause ++
    "group by " ++ GroupBy ++
    "order by t.\"date\", coalesce(t.parent, t.id), t.child_number;".

to_date(List) ->
    DateStrings = string:split(List, "-", all),
    list_to_tuple([to_int(DS) || DS <- DateStrings]).

to_int(List) when is_list(List) ->
    list_to_integer(List);
to_int(Bin) when is_binary(Bin) ->
    binary_to_integer(Bin).

fix_dates(List) when is_list(List) ->
    [fix_date(tuple_to_list(Rec)) || Rec <- List].

fix_date(List) when is_list(List) ->
    [fix_date(E) || E <- List];
fix_date({Y, M, D}) when is_integer(Y), is_integer(M), is_integer(D) ->
    {{Y, M, D}, {0, 0, 0}};
fix_date(Other) ->
    Other.

copy_parent(ParentId) ->
    ParentSql = "select "
                "       acct_type, "
                "       acct_num, "
                "       date, "
                "       posted, "
                "       cheq_num, "
                "       desc_1, "
                "       desc_2, "
                "       cad, "
                "       usd, "
                "       id parent "
                "from transaction "
                "where id = $1;",

    %io:format("Self = ~p~n", [self()]),
    [Child0] = budget_query:fetch(ParentSql, [ParentId]),
    %io:format(user, "Child0 = ~p~n", [Child0]),

    NumChildrenSql = "insert into transaction_child_number "
                     "(tx_id, child_number) "
                     "values "
                     "($1, 1) "
                     "on conflict (tx_id) "
                     "do update set child_number = transaction_child_number.child_number + 1 "
                     "returning transaction_child_number.child_number;",

    NumChildren = budget_query:update(NumChildrenSql, [ParentId]),
    %io:format(user, "NumChildren = ~p~n", [NumChildren]),

    {Date, _} = proplists:get_value(<<"date">>, Child0),
    Child1 = lists:keystore(<<"date">>,
                            1,
                            Child0,
                            {date, Date}),

    Child2 = case proplists:get_value(posted, Child1) of
                  {Posted, _} ->
                      lists:keystore(<<"posted">>,
                                     1,
                                     Child1,
                                     {posted, Posted});
                  _ ->
                     Child1
             end,

    Child3 = lists:keystore(<<"child_number">>,
                            1,
                            Child2,
                            {child_number, NumChildren}),

    Values = [V || {_K, V} <- Child3],
    %io:format(user, "Values = ~p~n", [Values]),

    InsertSql = "insert into transaction "
                "(id, acct_type, acct_num, date, "
                " posted, cheq_num, desc_1, desc_2, "
                "cad, usd, parent, child_number) "
                "values "
                "($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12);",

    HashedChild1 = [Hash | _] = hashed_record(Values),
    %io:format(user, "Parent ID = ~p~n", [ParentId]),
    %io:format(user, "HashedChild1 = ~p~n", [HashedChild1]),
    1 = budget_query:update(InsertSql, HashedChild1),
    Hash.


hashed_record(Rec) ->
    %io:format(user, "Rec = ~p~n", [Rec]),
    Strings = [serialize(Field) || Field <- Rec],
    %io:format(user, "Strings = ~p~n", [Strings]),
    String = lists:flatten(Strings),
    Hash = xxhash:hash64(String),
    [Hash | Rec].

serialize(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
serialize(Int) when is_integer(Int) ->
    i2l(Int);
serialize(Float) when is_float(Float) ->
    io_lib:format("~.2.0f", [Float]);
serialize(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
serialize({Y, M, D}) ->
    [i2l(Y), i2l(M), i2l(D)];
serialize({{Y, M, D}, {H, Min, S}}) ->
    [i2l(Y), i2l(M), i2l(D), i2l(H), i2l(Min), i2l(S)];
serialize(List) ->
    List.

delete_resource(Req, State) ->
    TxId = cowboy_req:binding(tx_id, Req),

    DeleteTxCatSql = "delete from transaction_category "
                     "where tx_id = $1; ",
    budget_query:update(DeleteTxCatSql, [TxId]),

    DeleteTxSql = "delete from transaction "
                  "where id = $1 "
                  "returning parent; ",
    Parent = budget_query:update(DeleteTxSql, [TxId]),

    DeleteTxChildNumSql = "delete from transaction_child_number txcn "
                          "where not exists "
                          "(select null "
                          " from transaction "
                          " where parent = txcn.tx_id)",
    budget_query:update(DeleteTxChildNumSql, []),

    Req1 = cowboy_req:set_resp_body(Parent, Req),

    {true, Req1, State}.

i2l(I) ->
    integer_to_list(I).

b2i(Bin) ->
    list_to_integer(binary_to_list(Bin)).
