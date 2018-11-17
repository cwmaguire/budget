-module(budget_db).

-export([connect/1]).
-export([close/1]).
-export([query/2]).
-export([query/3]).

connect(Opts) ->
    epgsql:connect(Opts).

close(Conn) ->
    epgsql:close(Conn).

query(Conn, Sql) ->
    query(Conn, Sql, []).

query(Conn, Sql, Params) ->
    epgsql:equery(Conn, Sql, [Params]).
