-module(budget_csv).

-export([parse/1]).

-define(IS_INT(I), I > 47, I < 58).
-define(IS_DATE_SEP(S), S == $-; S == $/).

parse(Csv) ->
    [_Header | Lines] = binary:split(Csv, <<"\r\n">>, [global]),
    Records = [record(L) || L <- Lines, size(L) > 0],
    [[parse_value(F) || F <- Rec] || Rec <- Records].

record(Bin) ->
    record(none, Bin, _Field = <<>>, _Fields = []).

%% F = Field, Fs = Fields
record(none, <<$,, Bin/binary>>, _F, Fs) ->
    record(none, Bin, <<>>, [null | Fs]);
record(none, <<I, Bin/binary>>, _F, Fs) when ?IS_INT(I); I == $- ->
    record(integer, Bin, <<I>>, Fs);
record(none, <<$", Bin/binary>>, _F, Fs) ->
    record(qstring, Bin, <<>>, Fs);
record(none, <<S, Bin/binary>>, _F, Fs) ->
    record(string, Bin, <<S>>, Fs);
record(_, <<>>, _, Fs) ->
    lists:reverse(Fs);

record(integer, <<I, Bin/binary>>, F, Fs) when ?IS_INT(I) ->
    record(integer, Bin, <<F/binary, I>>, Fs);
record(integer, <<$,, Bin/binary>>, F, Fs) ->
    Field = binary_to_integer(F),
    record(none, Bin, <<>>, [Field | Fs]);
record(integer, <<I>>, F, Fs) when ?IS_INT(I) ->
    Field = binary_to_integer(<<F/binary, I>>),
    lists:reverse([Field | Fs]);
record(integer, <<$., Bin/binary>>, F, Fs) ->
    record(float, Bin, <<F/binary, $.>>, Fs);
record(integer, <<S, Bin/binary>>, F, Fs) ->
    record(string, Bin, <<F/binary, S>>, Fs);

record(float, <<I, Bin/binary>>, F, Fs) when ?IS_INT(I) ->
    record(float, Bin, <<F/binary, I>>, Fs);
record(float, <<$,, Bin/binary>>, F, Fs) ->
    Field = binary_to_float(F),
    record(none, Bin, <<>>, [Field | Fs]);
record(float, <<I>>, F, Fs) when ?IS_INT(I) ->
    Field = binary_to_float(<<F/binary, I>>),
    lists:reverse([Field | Fs]);
record(float, <<S, Bin/binary>>, F, Fs) ->
    record(string, Bin, <<F/binary, S>>, Fs);

record(qstring, <<$", $,, Bin/binary>>, F, Fs) ->
    Field = binary_to_list(F),
    record(none, Bin, <<>>, [Field | Fs]);
record(qstring, <<S, Bin/binary>>, F, Fs) ->
    record(qstring, Bin, <<F/binary, S>>, Fs);

record(string, <<$,, Bin/binary>>, F, Fs) ->
    Field = binary_to_list(F),
    record(none, Bin, <<>>, [Field | Fs]);
record(string, <<S, Bin/binary>>, F, Fs) ->
    record(string, Bin, <<F/binary, S>>, Fs);
record(string, <<S>>, F, Fs) ->
    Field = binary_to_list(<<F/binary, S>>),
    lists:reverse([Field | Fs]).

parse_value([]) ->
    null;
parse_value(Val) when length(Val) < 11 ->
    case lists:all(fun date_char/1, Val) of
        true ->
            maybe_date(Val);
        false ->
            Val
    end;
parse_value(Val) ->
    Val.

maybe_date(Val) ->
    case lists:any(fun(X) -> not date_char(X) end, Val) of
        true ->
            Val;
        false ->
            date(Val)
    end.

date(DateString) ->
    case lists:any(fun is_slash/1, DateString) of
        true ->
            date(DateString, "/");
        false ->
            date(DateString, "-")
    end.

date(DateString, Sep) ->
    [D1, D2, D3] = string:split(DateString, Sep, all),
    case l2i(D1) > 12 of
        true ->
            {_Year = l2i(D1), _Month = l2i(D2), _Day = l2i(D3)};
        false ->
            {_Year = l2i(D3), _Month = l2i(D1), _Day = l2i(D2)}
    end.

l2i(List) ->
    list_to_integer(List).

date_char(I) when ?IS_INT(I); ?IS_DATE_SEP(I) ->
    true;
date_char(_) ->
    false.

is_slash($/) ->
    true;
is_slash(_) ->
    false.
