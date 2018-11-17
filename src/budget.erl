-module(budget).
-export([main/1]).
-export([import/2]).

main([Type, Path | _]) ->
    import(Type, Path).

import(Type, Path) ->
    application:start(xxhash),
    {ok, [DbOpts]} = file:consult("db_opts"),
    io:format(user, "DbOpts = ~p~n", [DbOpts]),
    {ok, File} = file:read_file(Path),
    %io:format(user, "File = ~p~n", [File]),
    [_Header | Records] = records(File),
    %io:format(user, "Records = ~p~n", [Records]),
    HashedRecords = hashed_records(Records),
    io:format(user, "HashedRecords = ~p~n", [HashedRecords]),
    %io:format(user, "HashedRecords = ~p~n", [HashedRecords]),
    application:stop(xxhash),
    insert(Type, HashedRecords, DbOpts).

records(Binary) ->
    Lines = binary:split(Binary, <<"\r\n">>, [global]),
    Records = [binary:split(L, <<",">>, [global]) || L <- Lines, L /= <<>>],
    [[binary_to_list(F) || F <- Line] || Line <- Records].

hashed_records(Records) ->
    [hashed_record(R) || R <- Records].

hashed_record(Rec) ->
    String = lists:flatten(Rec),
    Hash = xxhash:hash64(String),
    [Hash | Rec].

insert(rbc, HashedRecords, DbOpts) ->
    Sql = "insert into transaction "
          "(hash, acct_type, acct_num, date, posted, "
          " cheq_num, desc_1, desc_2, cad, usd) "
          "values "
          "($1, $2, $3, $4, $5, $6, $7, $8, $9, $10); ",
    Conn = budget_db:connect(DbOpts),
    [budget_db:query(Sql, R, DbOpts) || R <- HashedRecords],
    budget_db:close(Conn).
