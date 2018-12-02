-module(budget_web_category_h).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

%% Custom callbacks.
-export([category_get/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, category_get}
     ], Req, State}.

resource_exists(Req, State) ->
    {true, Req, State}.

category_get(Req, State) ->
    QsVals = cowboy_req:parse_qs(Req),
    {_, Callback} = lists:keyfind(<<"callback">>, 1, QsVals),

    Sql = "select id, name "
          "from category "
          "order by lower(name); ",

    Script = budget_query:fetch_jsonp(Sql, [], Callback),

    {Script, Req, State}.
