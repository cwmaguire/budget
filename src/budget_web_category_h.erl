-module(budget_web_category_h).

%% Standard callbacks.
-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
%-export([resource_exists/2]).

%% Custom callbacks.
-export([category_get/2]).
-export([category_post/2]).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, category_get}
     ], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, category_post}],
        Req, State}.

category_post(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            {{true, <<"/whatever">>}, Req, State};
        _ ->
            {true, Req, State}
    end.

category_get(Req, State) ->
    fetch_categories(Req, State).

fetch_categories(Req, State) ->
    QsVals = cowboy_req:parse_qs(Req),
    {_, Callback} = lists:keyfind(<<"callback">>, 1, QsVals),

    Sql = "select id, name "
          "from category "
          "order by lower(name); ",

    Script = budget_fetch:fetch(Sql, [], Callback),

    {Script, Req, State}.
