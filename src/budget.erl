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
    [_Header | Records] = budget_csv:parse(File),
    io:format(user, "Records = ~p~n", [Records]),
    HashedRecords = hashed_records(Records),
    %io:format(user, "HashedRecords = ~p~n", [HashedRecords]),
    %io:format(user, "HashedRecords = ~p~n", [HashedRecords]),
    application:stop(xxhash),
    insert(Type, HashedRecords, DbOpts).

hashed_records(Records) ->
    [hashed_record(R) || R <- Records].

hashed_record(Rec) ->
    io:format(user, "Hashing ~p~n", [Rec]),

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
    %io:format(user, "Conn = ~p~n", [Conn]),
    X = gen_server:call(Conn, {command,
                               epgsql_cmd_parse,
                               {"", Sql, []}},
                        infinity),
    io:format(user, "X = ~p~n", [X]),
    [budget_db:query(Conn, Sql, R) || R <- ParsedRecs],
    budget_db:close(Conn).

parse(rbc, Rec) ->
    io:format(user, "Rec = ~p~n", [Rec]),
    [Hash, Type, Acct, Date, ChequeNum, Desc1, Desc2, Cad, Usd] = Rec,
    Acct1 = to_list(Acct),
    Rec1 = [Hash, Type, Acct1, Date, ChequeNum, Desc1, Desc2, Cad, Usd],
    [normalize(Field) || Field <- Rec1].

to_list(List) when is_list(List) ->
    List;
to_list(Int) when is_integer(Int) ->
    integer_to_list(Int).

% to_date("") ->
%     null;
% to_date(List) ->
%     [M, D, Y] = string:split(List, "/", all),
%     {Y, M, D}.

% to_int(null) ->
%     null;
% to_int(List) ->
%     list_to_integer(List).

% to_float(null) ->
%     null;
% to_float(List) ->
%     case has_decimal(List) of
%         true ->
%             list_to_float(List);
%         false ->
%             list_to_float(List ++ ".0")
%     end.

% has_decimal("") ->
%     false;
% has_decimal("." ++ _) ->
%     true;
% has_decimal([_ | Rest]) ->
%     has_decimal(Rest).

normalize("") ->
    null;
normalize(Val) ->
    Val.
