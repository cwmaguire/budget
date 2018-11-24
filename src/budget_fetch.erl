-module(budget_fetch).

-export([fetch/3]).
-export([fetch/4]).

fetch(Sql, Params, Callback) ->
    fetch(Sql, Params, Callback, fun tuples_to_lists/1).

fetch(Sql, Params, Callback, Transformer) ->
    {ok, Conn} = budget_db:connect(),
    {ok, Cols, Records} = budget_db:query(Conn, Sql, Params),
    ColNames = [Name || {column, Name, _, _, _, _, _} <- Cols],
    io:format(user, "Cols = ~p~n", [Cols]),
    budget_db:close(Conn),
    Tuples = [lists:zip(ColNames, Rec) || Rec <- Transformer(Records)],
    Json = jsx:encode(Tuples),
    Script = <<"function ",
             Callback/binary,
             "(){return ", Json/binary, ";};">>,
    Script.

tuples_to_lists(Tuples) ->
    [tuple_to_list(Tuple) || Tuple <- Tuples].
