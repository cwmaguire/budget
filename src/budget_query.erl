-module(budget_query).

-export([fetch/2]).
-export([fetch_jsonp/3]).
-export([fetch_value/2]).
-export([update/2]).

fetch(Sql, Params) ->
    {ok, Conn} = budget_db:connect(),
    {ok, Cols, Records} = budget_db:query(Conn, Sql, Params),
    ColNames = [Name || {column, Name, _, _, _, _, _} <- Cols],
    budget_db:close(Conn),
    _Tuples = [lists:zip(ColNames, Rec) || Rec <- fix_dates(Records)].

fetch_jsonp(Sql, Params, Callback) ->
    Tuples = fetch(Sql, Params),
    Json = jsx:encode(Tuples),
    Script = <<"function ",
             Callback/binary,
             "(){return ", Json/binary, ";};">>,
    Script.

fetch_value(Sql, Params) ->
    {ok, Conn} = budget_db:connect(),
    {ok, _Cols, Records} = budget_db:query(Conn, Sql, Params),
    budget_db:close(Conn),
    case Records of
        [Record | _] ->
            [Value | _] = tuple_to_list(Record),
            Value;
        [] ->
            undefined
    end.

%tuples_to_lists(Tuples) ->
    %[tuple_to_list(Tuple) || Tuple <- Tuples].

update(Sql, Params) ->
    io:format("Calling update with Params: ~p~n", [Params]),
    {ok, Conn} = budget_db:connect(),
    {ok, NumUpdated} = budget_db:query(Conn, Sql, Params),
    budget_db:close(Conn),
    NumUpdated.

fix_dates(List) when is_list(List) ->
    [fix_date(tuple_to_list(Rec)) || Rec <- List].

fix_date(List) when is_list(List) ->
    [fix_date(E) || E <- List];
fix_date({Y, M, D}) when is_integer(Y), is_integer(M), is_integer(D) ->
    {{Y, M, D}, {0, 0, 0}};
fix_date(Other) ->
    Other.
