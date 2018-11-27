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
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, '*'}, category_post}],
        Req, State}.

%% TODO fill this in: check if the Tx and category exist
%% resource_exists(Req, _State) ->
%%     case cowboy_req:binding(tx, Req) of
%%         undefined ->
%%             {true, Req, index};
%%         Tx ->
%%             case valid_path(PasteID) and file_exists(PasteID) of
%%                 true -> {true, Req, PasteID};
%%                 false -> {false, Req, PasteID}
%%             end
%%     end.


category_post(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            category_add(Req, State);
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

    Script = budget_query:fetch(Sql, [], Callback),

    {Script, Req, State}.

category_add(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    KVs = kvs(Body),
    Tx = list_to_integer(proplists:get_value("tx", KVs)),
    Cat = list_to_integer(proplists:get_value("cat", KVs)),
    Sql = "insert into transaction_category "
          "(tx_id, cat_id) "
          "values "
          "($1, $2);",
    Return = budget_query:update(Sql, [Tx, Cat]),
    case Return of
        Count when Count > 0 ->
            {true, Req1, State};
        _ ->
            {false, Req1, State}
    end.

kvs(Binary) ->
    Bins = binary:split(Binary, [<<"&">>, <<"=">>], [global]),
    kvs(Bins, []).

kvs([KBin, VBin | Rest], KVs) ->
    K = binary_to_list(KBin),
    V = binary_to_list(VBin),
    kvs(Rest, [{K, V} | KVs]);
kvs(_, KVs) ->
    KVs.
