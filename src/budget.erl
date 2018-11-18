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
    Inserts = insert(Type, HashedRecords, DbOpts),
    categorize(Type, Inserts, DbOpts).

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
          "(id, acct_type, acct_num, date, "
          " cheq_num, desc_1, desc_2, cad, usd) "
          "values "
          "($1, $2, $3, $4, $5, $6, $7, $8, $9); ",
    {ok, Conn} = budget_db:connect(DbOpts),
    Results = [{budget_db:query(Conn, Sql, R), R} || R <- ParsedRecs],
    budget_db:close(Conn),
    Inserts = [Rec || {{ok, _}, Rec} <- Results],
    Errors = [{Err, Rec} || {{error, Err}, Rec} <- Results],
    [print_error(Err, Rec) || {Err, Rec} <- Errors],
    NumRecords = length(ParsedRecs),
    NumErrors = length(Errors),
    NumInserted = NumRecords - NumErrors,
    io:format(user,
              "Records: ~p, Inserted: ~p, Errors: ~p~n",
              [NumRecords, NumInserted, NumErrors]),
    Inserts.

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

categorize(Type, Recs, DbOpts) ->
    {ok, Conn} = budget_db:connect(DbOpts),
    RulesSql = "select * "
               "from category_rule "
               "order by \"order\";",
    {ok, _, RuleRecs} = budget_db:query(Conn, RulesSql),
    Rules = [{binary_to_list(M), CId} || {_, _, M, CId} <- RuleRecs],
    io:format(user, "Rules = ~p~n", [Rules]),
    CompiledRules0 = [{re:compile(M, [caseless]), CId} || {M, CId} <- Rules],
    io:format(user, "CompiledRules0 = ~p~n", [CompiledRules0]),
    CompiledRules = [{M, CId} || {{ok, M}, CId} <- CompiledRules0],
    Matches = matches(Type, Recs, CompiledRules),
    InsFun = fun insert_tx_category/3,
    Results = [{InsFun(Tx, Cat, Conn), {Tx, Cat}} || {Tx, Cat} <- Matches],
    NumRules = length(Rules),
    NumTx = length(Recs),
    NumMatches = length(Matches),
    io:format("Found ~p matches between ~p transactions and ~p rules~n",
              [NumMatches, NumTx, NumRules]),
    [print_error(Err, Rec) || {{error, Err}, Rec} <- Results],
    budget_db:close(Conn).

matches(rbc, Recs, Rules) ->
    %io:format(user, "Rules = ~p~n", [Rules]),
    Fields = [rbc_match_fields(Rec) || Rec <- Recs],
    %io:format(user, "Fields = ~p~n", [Fields]),
    Results = [{Id, re:run(Desc, M), Cat} || {Id, Desc} <- Fields,
                                             {M, Cat} <- Rules],
    io:format(user, "Results = ~p~n", [Results]),
    lists:usort([{Tx, Cat} || {Tx, {match, _}, Cat} <- Results]).

rbc_match_fields([Id, _, _, _, _, Desc1, Desc2, _, _]) ->
    {Id, lists:flatten([null_to_list(Desc1), " ", null_to_list(Desc2)])}.

null_to_list(null) ->
    "";
null_to_list(List) ->
    List.

insert_tx_category(Tx, Cat, Conn) ->
    Sql = "insert into transaction_category (tx_id, cat_id) "
          "values "
          "($1, $2) "
          "on conflict do nothing;",
    budget_db:query(Conn, Sql, [Tx, Cat]).


