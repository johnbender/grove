-module(grove_test).
-export([setup/0]).
-include_lib("stdlib/include/qlc.hrl"). 
-include_lib("eunit/include/eunit.hrl").


-record(shop, {item, quantity, cost}).
-record(cost, {name, price}).

-define(JSON_ITEM_COLUMN_ORANGES, 
	"{ \"query\" :
                [   {\"columns\" : [\"item\"]}, 
                    {\"operations\" :  [{\"eq\" : {\"item\" : { \"string\" : \"orange\"}}}]}, 
                    {\"order\": \"descending\" }
                ] 
         }").

grove_json_test_() ->
    { setup,
      local,
      fun setup/0,
      fun cleanup/1,
      fun(_result) ->         
	      [?_assert("[{\"item\":\"orange\"}]" == grove:post_query("shop", ?JSON_ITEM_COLUMN_ORANGES)),
	       ?_assert(2==2)]
      end
     }.


schema() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(shop,   [{attributes, record_info(fields, shop)}]),
    mnesia:create_table(cost,   [{attributes, record_info(fields, cost)}]),
    mnesia:stop().

start() ->
    mnesia:start(),
    grove:start("config/config.txt"),
    mnesia:wait_for_tables([shop,cost,design], 20000).

stop() ->
    mnesia:stop(),
    mnesia:delete_schema(node()).

tables() ->
    mnesia:clear_table(shop),
    mnesia:clear_table(cost),
    F = fun() ->
		lists:foreach(fun mnesia:write/1, test_data())
	end,
    mnesia:transaction(F).

test_data() ->
    [
     {shop, apple,   20,   2.3},
     {shop, "orange",  150,  3.8},
     {shop, pear,    200,  3.6},
     {shop, banana,  420,  4.5},
     {shop, potato,  2456, 1.2},
     {cost, apple,   1.5},
     {cost, orange,  2.4},
     {cost, pear,    2.2},
     {cost, banana,  1.5},
     {cost, potato,  0.6}
    ].

setup() ->
    schema(),
    start(),
    tables(),
    ok.

cleanup(_result) ->
    stop(),
    ok.

