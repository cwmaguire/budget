-module(budget_fetch).

-export([fetch/4]).
-export([fetch/5]).

fetch(Sql, FieldNames, Params, Callback) ->
    fetch(Sql, FieldNames, Params, Callback, fun tuples_to_lists/1).

fetch(Sql, FieldNames, Params, Callback, Transformer) ->
    {ok, Conn} = budget_db:connect(),
    {ok, Cols, Records} = budget_db:query(Conn, Sql, Params),
    io:format(user, "Cols = ~p~n", [Cols]),
    budget_db:close(Conn),
    Tuples = [lists:zip(FieldNames, Rec) || Rec <- Transformer(Records)],
    Json = jsx:encode(Tuples),
    Script = <<"function ",
             Callback/binary,
             "(){return ", Json/binary, ";};">>,
    Script.

tuples_to_lists(Tuples) ->
    [tuple_to_list(Tuple) || Tuple <- Tuples].
