%% @private
-module(budget_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).
-export([etag/3]).

-define(no_etag, [{etag, false}]).
%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/category", budget_web_category_h, []},
			{"/", budget_web_h, []},
            {"/assets/[...]", cowboy_static,
             {priv_dir, budget, "static", [{etag, budget_app, etag}]}}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	budget_sup:start_link().

stop(_State) ->
	ok.

etag(_, _, _) ->
    {strong, integer_to_list(os:system_time())}.
