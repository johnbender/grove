-module(grove_test).
-export([setup/0]).
-include_lib("stdlib/include/qlc.hrl"). 
-include_lib("eunit/include/eunit.hrl").


-record(shop, {item, quantity, cost}).

-define(RSLT_ITEM_COLUMN_ORANGES, "[{\"item\":\"orange\"}]").
-define(QRY_ITEM_COLUMN_ORANGES, 
	"{ \"query\" :
                [   {\"columns\" : [\"item\"]}, 
                    {\"operations\" :  [{\"eq\" : {\"item\" : { \"string\" : \"orange\"}}}]}, 
                    {\"order\": \"descending\" }
                ] 
         }").

-define(RSLT_ALL_COLUMN_ATOM_PEAR, "[{\"item\":\"pear\",\"quantity\":200,\"cost\":3.6}]").
-define(QRY_ALL_COLUMN_ATOM_PEAR, 
	"{ \"query\" :
                [   {\"columns\" : []}, 
                    {\"operations\" :  [{\"eq\" : {\"item\" : { \"atom\" : \"pear\"}}}]}, 
                    {\"order\": \"descending\" }
                ] 
         }").

%pear is defined as an atom in the test data set, so it will not match
-define(RSLT_ALL_COLUMN_PEAR, "[]").
-define(QRY_ALL_COLUMN_PEAR, 
	"{ \"query\" :
                [   {\"columns\" : []}, 
                    {\"operations\" :  [{\"eq\" : {\"item\" : \"pear\"}}]}, 
                    {\"order\": \"descending\" }
                ] 
         }").

%%strings come after atoms 
-define(RSLT_ITEM_COLUMN_NOOP_ASC, "[{\"item\":\"apple\"},{\"item\":\"pear\"},{\"item\":\"potato\"},{\"item\":\"banana\"},{\"item\":\"orange\"}]").
-define(QRY_ITEM_COLUMN_NOOP_ASC, 
	"{ \"query\" :
                [   {\"columns\" : [\"item\"]}, 
                    {\"operations\" :  []}, 
                    {\"order\": \"ascending\" }
                ] 
         }").

%%strings come before atoms 
-define(RSLT_ITEM_COLUMN_NOOP_DESC, "[{\"item\":\"orange\"},{\"item\":\"banana\"},{\"item\":\"potato\"},{\"item\":\"pear\"},{\"item\":\"apple\"}]").
-define(QRY_ITEM_COLUMN_NOOP_DESC, 
	"{ \"query\" :
                [   {\"columns\" : [\"item\"]}, 
                    {\"operations\" :  []}, 
                    {\"order\": \"descending\"}
                ] 
         }").

-define(RSLT_ITEM_COLUMN_LT, "[{\"item\":\"potato\"}]").
-define(QRY_ITEM_COLUMN_LT, 
	"{ \"query\" :
                [   {\"columns\" : [\"item\"]}, 
                    {\"operations\" :  [{\"lt\" : {\"cost\" : 2.2}}]}, 
                    {\"order\": [] }
                ] 
         }").


grove_json_test_() ->
    { setup,
      local,
      fun setup/0,
      fun cleanup/1,
      fun(_result) ->         
	      [?_assert(?RSLT_ITEM_COLUMN_ORANGES == grove:post_query("shop", ?QRY_ITEM_COLUMN_ORANGES)),
	       ?_assert(?RSLT_ALL_COLUMN_ATOM_PEAR == grove:post_query("shop", ?QRY_ALL_COLUMN_ATOM_PEAR)),
	       ?_assert(?RSLT_ALL_COLUMN_PEAR == grove:post_query("shop", ?QRY_ALL_COLUMN_PEAR)),
	       ?_assert(?RSLT_ITEM_COLUMN_NOOP_ASC == grove:post_query("shop", ?QRY_ITEM_COLUMN_NOOP_ASC)),
	       ?_assert(?RSLT_ITEM_COLUMN_NOOP_DESC == grove:post_query("shop", ?QRY_ITEM_COLUMN_NOOP_DESC)),
	       ?_assert(?RSLT_ITEM_COLUMN_LT == grove:post_query("shop", ?QRY_ITEM_COLUMN_LT))
	      ]
      end
     }.

test_data() ->
    [
     {shop, apple,   20,   2.3},
     {shop, "orange",  150,  3.8},
     {shop, pear,    200,  3.6},
     {shop, "banana",  420,  4.5},
     {shop, potato,  2456, 1.2}
    ].

schema() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(shop,   [{attributes, record_info(fields, shop)}]),
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

setup() ->
    schema(),
    start(),
    tables(),
    ok.

cleanup(_result) ->
    stop(),
    ok.

