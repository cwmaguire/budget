-module(budget).
-export([main/1]).
-export([import/2]).

main([Type, Path | _]) ->
    import(Type, Path).

import(Type, Path) ->
    application:start(xxhash),
    {ok, [DbOpts]} = file:consult("db_opts"),
    {ok, File} = file:read_file(Path),
    [_Header | Records] = budget_csv:parse(File),
    HashedRecords = hashed_records(Records),
    application:stop(xxhash),
    insert(Type, HashedRecords, DbOpts).

hashed_records(Records) ->
    [hashed_record(R) || R <- Records].

hashed_record(Rec) ->
    Strings = [serialize(Field) || Field <- Rec],
    String = lists:flatten(Strings),
    Hash = xxhash:hash64(String),
    [Hash | Rec].

serialize(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
serialize(Int) when is_integer(Int) ->
    i2l(Int);
serialize(Float) when is_float(Float) ->
    io_lib:format("~.2.0f", [Float]);
serialize({Y, M, D}) ->
    [i2l(Y), i2l(M), i2l(D)];
serialize(List) ->
    List.

i2l(I) ->
    integer_to_list(I).

insert(rbc, HashedRecords, DbOpts) ->
    ParsedRecs = [parse(rbc, R) || R <- HashedRecords],
    Sql = "insert into transaction "
          "(hash, acct_type, acct_num, date, "
          " cheq_num, desc_1, desc_2, cad, usd) "
          "values "
          "($1, $2, $3, $4, $5, $6, $7, $8, $9); ",
    {ok, Conn} = budget_db:connect(DbOpts),
    Result = [{budget_db:query(Conn, Sql, R), R} || R <- ParsedRecs],
    [print_error(Err, Rec) || {{error, Err}, Rec} <- Result],
    budget_db:close(Conn).

parse(rbc, Rec) ->
    [Hash, Type, Acct, Date, ChequeNum, Desc1, Desc2, Cad, Usd] = Rec,
    Acct1 = to_list(Acct),
    Rec1 = [Hash, Type, Acct1, Date, ChequeNum, Desc1, Desc2, Cad, Usd],
    [normalize(Field) || Field <- Rec1].

to_list(List) when is_list(List) ->
    List;
to_list(Int) when is_integer(Int) ->
    integer_to_list(Int).

normalize("") ->
    null;
normalize(Val) ->
    Val.

%{error,error,<<"23505">>,unique_violation,
%       <<"duplicate key value violates unique constraint \"transaction_hash_idx\"">>,
%       [{constraint_name,<<"transaction_hash_idx">>},
%        {detail,<<"Key (hash)=(12127115697651675763) already exists.">>},
%        {file,<<"nbtinsert.c">>},
%        {line,<<"424">>},
%        {routine,<<"_bt_check_unique">>},
%        {schema_name,<<"public">>},
%        {table_name,<<"transaction">>}]}]
print_error({error,
             error,
             _,
             unique_violation,
             Error,
             [_, Detail | _]},
            Record) ->
    io:format("Error: duplicate record:~n    ~p~n    ~p~n    Record: ~p~n",
              [Error, Detail, Record]);
print_error(Error, Record) ->
    io:format("Error:~n    ~p~n    Record: ~p~n", [Error, Record]).
