-module(budget_web_transaction_category_h).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).

%% Custom callbacks.
-export([category_get/2]).
-export([category_post/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, category_get}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, category_post}],
        Req, State}.

resource_exists(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            QsVals = cowboy_req:parse_qs(Req),
            {_, Tx} = lists:keyfind(<<"tx">>, 1, QsVals),
            {tx_exists(b2i(Tx)), Req, State};
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req),
            KVs = kvs(Body),
            Tx = list_to_integer(proplists:get_value("tx", KVs)),
            Cat = list_to_integer(proplists:get_value("cat", KVs)),
            {tx_exists(Tx) and cat_exists(Cat), Req1, [{kvs, KVs} | State]};
        _ ->
            {true, Req, State}
    end.

tx_exists(TxId) ->
    Sql = "select id "
          "from transaction t "
          "where t.id = $1; ",
    TxId == b2i(budget_query:fetch_value(Sql, [TxId])).

cat_exists(CatId) ->
    Sql = "select id "
          "from category c "
          "where c.id = $1; ",

    case budget_query:fetch_value(Sql, [CatId]) of
        Int when is_integer(Int) ->
            CatId == Int;
        String when is_list(String) ->
            CatId == list_to_integer(String);
        Binary when is_binary(Binary) ->
            CatId == b2i(Binary)
    end.

category_get(Req, State) ->
    QsVals = cowboy_req:parse_qs(Req),
    {_, Tx} = lists:keyfind(<<"tx">>, 1, QsVals),

    Sql = "select string_agg(c.name || '||' || tc.id, ', ') categories "
          "from transaction_category tc "
          "inner join category c "
          "  on tc.cat_id = c.id "
          "where tc.tx_id = $1; ",

    Categories = budget_query:fetch_value(Sql, [b2i(Tx)]),
    Value = case Categories of
                null ->
                    "";
                _ ->
                    Categories
            end,
    {Value, Req, State}.

category_post(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            category_add(Req, State);
        _ ->
            {true, Req, State}
    end.

category_add(Req, State) ->
    KVs = proplists:get_value(kvs, State),
    Tx = list_to_integer(proplists:get_value("tx", KVs)),
    Cat = list_to_integer(proplists:get_value("cat", KVs)),
    Sql = "insert into transaction_category "
          "(tx_id, cat_id) "
          "select $1, $2 "
          "where not exists "
          "(select null "
          " from transaction_category "
          " where tx_id = $1 "
          " and cat_id = $2); ",
    Return = budget_query:update(Sql, [Tx, Cat]),
    case Return of
        Count when Count > 0 ->
            {true, Req, State};
        _ ->
            {false, Req, State}
    end.

kvs(Binary) ->
    Bins = binary:split(Binary, [<<"&">>, <<"=">>], [global]),
    kvs(Bins, []).

kvs([KBin, VBin | Rest], KVs) ->
    K = binary_to_list(KBin),
    V = binary_to_list(VBin),
    kvs(Rest, [{K, V} | KVs]);
kvs(_, KVs) ->
    KVs.

delete_resource(Req, State) ->
    TxCatId = cowboy_req:binding(tx_cat_id, Req),

    Sql = "delete from transaction_category "
          "where id = $1; ",
    Return = budget_query:update(Sql, [TxCatId]),
    case Return of
        Count when Count > 0 ->
            {true, Req, State};
        _ ->
            {false, Req, State}
    end.

b2i(Bin) ->
    list_to_integer(binary_to_list(Bin)).
