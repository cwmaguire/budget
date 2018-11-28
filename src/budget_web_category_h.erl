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
    QsVals = cowboy_req:parse_qs(Req),
    case lists:keyfind(<<"tx">>, 1, QsVals) of
        {_, Tx} ->
            fetch_categories_by_tx(Tx, Req, State);
        false ->
            fetch_categories(Req, State)
    end.

fetch_categories(Req, State) ->
    QsVals = cowboy_req:parse_qs(Req),
    {_, Callback} = lists:keyfind(<<"callback">>, 1, QsVals),

    Sql = "select id, name "
          "from category "
          "order by lower(name); ",

    Script = budget_query:fetch(Sql, [], Callback),

    {Script, Req, State}.

fetch_categories_by_tx(Tx, Req, State) ->
    io:format("fetch_categories_by_tx with tx: ~p~n", [Tx]),
    Sql = "select string_agg(c.name, ', ') categories "
          "from transaction_category tc "
          "inner join category c "
          "  on tc.cat_id = c.id "
          "where tc.tx_id = $1; ",

    Categories = budget_query:fetch_value(Sql, [b2i(Tx)]),
    Value = case Categories of
                null ->
                    io:format("No categories for tx ~p~n", [Tx]),
                    "";
                _ ->
                    io:format("Categories for tx ~p ~p~n", [b2i(Tx), Categories]),
                    Categories
            end,
    io:format(user, "Value = ~p~n", [Value]),

    {Value, Req, State}.

category_add(Req, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req),
    KVs = kvs(Body),
    Tx = list_to_integer(proplists:get_value("tx", KVs)),
    Cat = list_to_integer(proplists:get_value("cat", KVs)),
    Sql = "insert into transaction_category "
          "(tx_id, cat_id) "
          "select $1, $2 "
          "where not exists "
          "(select null "
          " from transaction_category "
          " where tx_id = $1 "
          " and cat_id = $2); ",
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

b2i(Bin) ->
    list_to_integer(binary_to_list(Bin)).
