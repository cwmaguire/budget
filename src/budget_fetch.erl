-module(budget_fetch).

-export([fetch/4]).
-export([fetch/5]).

fetch(Sql, FieldNames, Params, Callback) ->
    fetch(Sql, FieldNames, Params, Callback, fun identity/1).

fetch(Sql, FieldNames, Params, Callback, Transformer) ->
    {ok, Conn} = budget_db:connect(),
    {ok, Cols, Records0} = budget_db:query(Conn, Sql, Params),
    io:format(user, "Cols = ~p~n", [Cols]),
    budget_db:close(Conn),
    Records1 = [Transformer(Rec) || Rec <- Records0],
    Tuples = [lists:zip(FieldNames, tuple_to_list(Rec)) || Rec <- Records1],
    Json = jsx:encode(Tuples),
    Script = <<"function ",
             Callback/binary,
             "(){return ", Json/binary, ";};">>,
    Script.

identity(X) ->
    X.
