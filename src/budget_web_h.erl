-module(budget_web_h).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
%-export([resource_exists/2]).

%% Custom callbacks.
-export([budget_post/2]).
-export([budget_get/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"application">>, <<"json">>, []}, budget_get}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"json">>, '*'}, budget_post}],
		Req, State}.

%resource_exists(Req, _State) ->
%	case cowboy_req:binding(paste_id, Req) of
%		undefined ->
%			{true, Req, index};
%		PasteID ->
%			case valid_path(PasteID) and file_exists(PasteID) of
%				true -> {true, Req, PasteID};
%				false -> {false, Req, PasteID}
%			end
%	end.

budget_post(Req, State) ->
	case cowboy_req:method(Req) of
		<<"POST">> ->
			{{true, <<"/whatever">>}, Req, State};
		_ ->
			{true, Req, State}
	end.

%budget_get(Req, #{from_date := From, to_date := To}) ->
budget_get(Req, State) ->
    QsVals = cowboy_req:parse_qs(Req),
    {_, Callback} = lists:keyfind(<<"callback">>, 1, QsVals),
    {_, RawFrom} = lists:keyfind(<<"from">>, 1, QsVals),
    {_, RawTo} = lists:keyfind(<<"to">>, 1, QsVals),
    From = to_date(RawFrom),
    To = to_date(RawTo),
    Sql = "select cad, desc_1 from transaction "
          "where date between $1 and $2; ",
    {ok, Conn} = budget_db:connect(),
    {ok, _Cols, Txs} = budget_db:query(Conn, Sql, [From, To]),
    Tuples = [[{cad, Cad}, {desc, Desc}] || {Cad, Desc} <- Txs],
    budget_db:close(Conn),
    Json = jsx:encode(Tuples),
    Script = <<"function ",
             Callback/binary,
             "(){return ", Json/binary, ";};">>,
	{Script, Req, State}.

to_date(List) ->
    DateStrings = string:split(List, "-", all),
    list_to_tuple([to_int(DS) || DS <- DateStrings]).

to_int(List) when is_list(List) ->
    list_to_integer(List);
to_int(Bin) when is_binary(Bin) ->
    binary_to_integer(Bin).
