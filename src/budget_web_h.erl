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
budget_get(Req, _) ->
    From = {2018, 10, 1},
    To = {2018, 11, 1},
    Sql = "select cad, desc_1 from transaction "
          "where date between $1 and $2; ",
    {ok, Conn} = budget_db:connect(),
    {ok, _Cols, Txs} = budget_db:query(Conn, Sql, [From, To]),
    Tuples = [[{cad, Cad}, {desc, Desc}] || {Cad, Desc} <- Txs],
    budget_db:close(Conn),
    Json = jsx:encode(Tuples),
	{Json, Req, index}.
