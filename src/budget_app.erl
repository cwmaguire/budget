%% @private
-module(budget_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", budget_web_h, []},
            {"/index.html", cowboy_static, {priv_file, budget, "index.html"}}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	budget_sup:start_link().

stop(_State) ->
	ok.
