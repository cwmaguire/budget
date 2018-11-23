-module(budget_web_h).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
%-export([resource_exists/2]).

%% Custom callbacks.
-export([budget_post/2]).
-export([fetch/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, fetch}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, budget_post}],
		Req, State}.

%resource_exists(Req, _State) ->
%	case cowboy_req:binding(paste_id, Req) of
%		undefined ->
%			{true, Req, index};
%		PasteID ->
%			case valid_path(PasteID) and file_exists(PasteID) of
%				true -> {true, Req, PasteID};
%				false -> {false, Req, PasteID}
%			end
%	end.

budget_post(Req, State) ->
	case cowboy_req:method(Req) of
		<<"POST">> ->
			{{true, <<"/whatever">>}, Req, State};
		_ ->
			{true, Req, State}
	end.

%budget_get(Req, #{from_date := From, to_date := To}) ->
fetch(Req, State) ->
    QsVals = cowboy_req:parse_qs(Req),
    case lists:keyfind(<<"type">>, 1, QsVals) of
        {_, <<"transaction">>} ->
            fetch_transactions(QsVals, Req, State);
        {_, <<"category">>} ->
            fetch_categories(QsVals, Req, State);
        _ ->
            {<<>>, Req, State}
    end.

fetch_transactions(QsVals, Req, State) ->
    {_, Callback} = lists:keyfind(<<"callback">>, 1, QsVals),
    {_, RawFrom} = lists:keyfind(<<"from">>, 1, QsVals),
    {_, RawTo} = lists:keyfind(<<"to">>, 1, QsVals),
    From = to_date(RawFrom),
    To = to_date(RawTo),
    io:format("From date is ~p~n", [From]),
    io:format("To date is ~p~n", [To]),

    FieldNames = [id, acct_type, acct_num, date, posted,
                  cheq_num, desc_1, desc_2, cad, usd],
    Sql = "select * "
          "from transaction "
          "where date between $1 and $2; ",
    {ok, Conn} = budget_db:connect(),
    {ok, _Cols, Txs} = budget_db:query(Conn, Sql, [From, To]),
    Txs1 = fix_dates(Txs),
    Tuples = [lists:zip(FieldNames, Tx) || Tx <- Txs1],
    budget_db:close(Conn),
    Json = jsx:encode(Tuples),
    Script = <<"function ",
             Callback/binary,
             "(){return ", Json/binary, ";};">>,
	{Script, Req, State}.

fetch_categories(QsVals, Req, State) ->
    QsVals = cowboy_req:parse_qs(Req),
    {_, Callback} = lists:keyfind(<<"callback">>, 1, QsVals),

    FieldNames = [id, name],
    Sql = "select id, name "
          "from category; ",
    {ok, Conn} = budget_db:connect(),
    {ok, _Cols, Cats} = budget_db:query(Conn, Sql, []),
    budget_db:close(Conn),
    Tuples = [lists:zip(FieldNames, tuple_to_list(Cat)) || Cat <- Cats],
    Json = jsx:encode(Tuples),
    Script = <<"function ",
             Callback/binary,
             "(){return ", Json/binary, ";};">>,
	{Script, Req, State}.

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
    %io:format("Replacing date: {~p, ~p, ~p}~n", [Y, M, D]),
    {{Y, M, D}, {0, 0, 0}};
fix_date(Other) ->
    Other.
