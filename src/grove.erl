-module(grove).
-export([get_query/3,
	 post_query/2,
	 start/0,
         start/1,
	 update_config/1
	]).
-compile(export_all).

-define(NOTEST, 1).
-define(ERROR_STATUS, {status, 400}).
-define(OBJECT_NOT_AVAILABLE, "\"" ++ Object ++ " is not available for querying.\"").
-define(FUNCTION_NOT_IMP, "\"" ++  Function ++ " is not implemented.\"").
-define(PARSED_JSON_STRUCTURE, {struct,
				[{"query",
				  {array,
				   [{struct,[{"columns",Columns}]},
				    {struct,[{"operations", Ops}]},
				    {struct,[{"order", Ord}]}]}}]} ). 

-include_lib("stdlib/include/qlc.hrl"). 
-include_lib("eunit/include/eunit.hrl").

%%-----------------------------------------------------------------------------------------------
%% Function:    get_query
%% @doc called from out/1 when a GET request is processed with /object/action uri
%%              and possibly /object/action/param1/param2/etc/. Compiles fun that calls
%%              the Function in the defined module with the Object and Params <br/>
%% Arguments:   Obj: should represent a table in the data store being queried.
%%              Act: MUST represent a function in the CMod module
%%              Params: represents the rest of the URI path elements or an empty set
%%-----------------------------------------------------------------------------------------------
get_query(Obj, Act, Params) ->

    {CMod, DMod, PObjs} = read_config(),
    Function = list_to_atom(string:to_lower(Act)),
    Object = string:to_lower(Obj),
    {value, {exports, Exports}} = lists:keysearch(exports, 1, CMod:module_info()),

    %make sure the action is implemented, the object exists, and the object is permitted
    case {DMod:object_exists(Object), lists:member(Object, PObjs), lists:member({Function, 2}, Exports)} of
	{false, _, _} -> ?OBJECT_NOT_AVAILABLE;
	{_, false, _} -> ?OBJECT_NOT_AVAILABLE;
	{_, _, false} -> ?FUNCTION_NOT_IMP;
	{true, true, true} -> CMod:Function(Object, Params)
    end.

%%-----------------------------------------------------------------------------------------------
%% Function:    post_query
%% Description: called from out/1 when a POST request is processed with /object URI
%%-----------------------------------------------------------------------------------------------
post_query(Obj, JSON) -> 
    
    {_, DMod, PObjs} = read_config(),
    Object = string:to_lower(Obj),
    Decoded = try mochijson:decode(JSON) catch error : _ -> fail end,
    Exists = DMod:object_exists(Object),
    Permitted = lists:member(Object, PObjs),

    case {Exists, Permitted, Decoded} of
	{false, _ , _} -> ?OBJECT_NOT_AVAILABLE;
	{_, false, _ } -> ?OBJECT_NOT_AVAILABLE;
	{true, true, ?PARSED_JSON_STRUCTURE} ->
	    Result = DMod:run_query({qry, 
				     {table, Object}, 
				     {columns, Columns}, 
				     {operations, Ops}, 
				     {order, Ord}}),
	    DMod:format_json(Result, Object, Columns);
	_other -> invalid_json() 
    end.

invalid_json() ->
     "\"Invalid JSON String. Your JSON must take the format: " ++
				  "{\"query\" : " ++ 
				  "{\"columns\" : []}," ++ 
				  "{\"operations\" :  [] }," ++ 
				  "{\"order\": [] }] }\"".



%%-----------------------------------------
%%           Configuration Setup
%%-----------------------------------------


-record(config_entry, {name, value}).

start(ConfigLoc) ->
    mnesia:start(),
%% needs to check for errors in the yaws config file file
    _ = yaws:start(),
    update_config(ConfigLoc).


start() ->
    start(query_config(config_location)).

consult_config(ConfigLoc) ->
    {ok, Config} = file:consult(ConfigLoc),
    Config.


verify_config(Table) when is_atom(Table) ->
    Tables = mnesia:system_info(tables),
    case lists:member(Table, Tables) of
	true ->
	    ok;
	false -> 
	    mnesia:create_table(Table, [{attributes, record_info(fields, config_entry)}, {ram_copies, [node()]}]),
	    ok	 
    end.


write_config(_table, []) -> ok;

write_config(Table, [{Name, Value}|T]) when is_atom(Table) ->
    Entry = {grove_config, Name, Value},
    mnesia:transaction(fun() -> mnesia:write(Entry) end),
    write_config(Table, T).
    

update_config(ConfigLoc) ->
    [{config_table_name, ConfTableName}|Config] = consult_config(ConfigLoc),
    ok = verify_config(ConfTableName),
    ok = write_config(ConfTableName, Config ++ [{config_location, ConfigLoc}]).


query_config(EntryName) when is_atom(EntryName) ->
    T = fun() -> 
		qlc:e(qlc:q([ X || X <- mnesia:table(grove_config), X#config_entry.name =:= EntryName]))
	end,
    {atomic, Rows} = mnesia:transaction(T),
    case Rows of
	[] -> io:format("There is no entry in the config mnesia table matching" ++ atom_to_list(EntryName));
	[{grove_config, EntryName, Result}] -> Result
    end.

read_config() ->
    { query_config(custom_mod),
      query_config(data_adapter_mod),
      grove_util:all_lower_strings(query_config(objects)) }.





%%-----------------------------------------
%%    Integration Tests with Mnesia
%%-----------------------------------------



%%All the folowing macros should be usable for integration testing with other adapters, ie MySQL
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

-define(RSLT_ITEM_COLUMN_LTE, "[{\"item\":\"potato\"},{\"item\":\"apple\"}]").
-define(QRY_ITEM_COLUMN_LTE, 
	"{ \"query\" :
                [   {\"columns\" : [\"item\"]}, 
                    {\"operations\" :  [{\"lte\" : {\"cost\" : 2.3}}]}, 
                    {\"order\": \"descending\" }
                ] 
         }").

-define(RSLT_ITEM_COLUMN_GT, "[{\"item\":\"banana\"}]").
-define(QRY_ITEM_COLUMN_GT, 
	"{ \"query\" :
                [   {\"columns\" : [\"item\"]}, 
                    {\"operations\" :  [{\"gt\" : {\"cost\" : 4}}]}, 
                    {\"order\": \"descending\" }
                ] 
         }").

-define(RSLT_ITEM_COLUMN_GTE, "[{\"item\":\"orange\"},{\"item\":\"banana\"}]").
-define(QRY_ITEM_COLUMN_GTE, 
	"{ \"query\" :
                [   {\"columns\" : [\"item\"]}, 
                    {\"operations\" :  [{\"gte\" : {\"cost\" : 3.8}}]}, 
                    {\"order\": \"descending\" }
                ] 
         }").

-define(RSLT_ITEM_COLUMN_NEQ, "[{\"item\":\"orange\"},{\"item\":\"banana\"},{\"item\":\"potato\"},{\"item\":\"pear\"}]").
-define(QRY_ITEM_COLUMN_NEQ, 
	"{ \"query\" :
                [   {\"columns\" : [\"item\"]}, 
                    {\"operations\" :  [{\"neq\" : {\"item\" : {\"atom\": \"apple\"} }}]}, 
                    {\"order\": \"descending\" }
                ] 
         }").

-define(RSLT_REVERSE_COLUMN, "[{\"cost\":4.5,\"item\":\"banana\"}]").
-define(QRY_REVERSE_COLUMN,
	"{ \"query\" :
                [   {\"columns\" : [\"cost\" , \"item\"]}, 
                    {\"operations\" :  [{\"gt\" : {\"cost\" : 4}}]}, 
                    {\"order\": \"descending\" }
                ] 
         }").

grove_mnesia_test_() ->
    { setup,
      local,
      fun mnesia_setup/0,
      fun mnesia_cleanup/1,
      fun(_result) ->         
	      [?_assert(?RSLT_ITEM_COLUMN_ORANGES == grove:post_query("shop", ?QRY_ITEM_COLUMN_ORANGES)),
	       ?_assert(?RSLT_ALL_COLUMN_ATOM_PEAR == grove:post_query("shop", ?QRY_ALL_COLUMN_ATOM_PEAR)),
	       ?_assert(?RSLT_ALL_COLUMN_PEAR == grove:post_query("shop", ?QRY_ALL_COLUMN_PEAR)),
	       ?_assert(?RSLT_ITEM_COLUMN_NOOP_ASC == grove:post_query("shop", ?QRY_ITEM_COLUMN_NOOP_ASC)),
	       ?_assert(?RSLT_ITEM_COLUMN_NOOP_DESC == grove:post_query("shop", ?QRY_ITEM_COLUMN_NOOP_DESC)),
	       ?_assert(?RSLT_ITEM_COLUMN_LT == grove:post_query("shop", ?QRY_ITEM_COLUMN_LT)),
	       ?_assert(?RSLT_ITEM_COLUMN_LTE == grove:post_query("shop", ?QRY_ITEM_COLUMN_LTE)),
	       ?_assert(?RSLT_ITEM_COLUMN_GT == grove:post_query("shop", ?QRY_ITEM_COLUMN_GT)),
	       ?_assert(?RSLT_ITEM_COLUMN_GTE == grove:post_query("shop", ?QRY_ITEM_COLUMN_GTE)),
	       ?_assert(?RSLT_ITEM_COLUMN_NEQ == grove:post_query("shop", ?QRY_ITEM_COLUMN_NEQ)),
	       ?_assert(?RSLT_REVERSE_COLUMN == grove:post_query("shop", ?QRY_REVERSE_COLUMN))
	      ]
      end
     }.


-record(shop, {item, quantity, cost}).

test_data() ->
    [
     {shop, apple,   20,   2.3},
     {shop, "orange",  150,  3.8},
     {shop, pear,    200,  3.6},
     {shop, "banana",  420,  4.5},
     {shop, potato,  2456, 1.2}
    ].

test_schema() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(shop,   [{attributes, record_info(fields, shop)}]),
    mnesia:stop().

test_start() ->
    mnesia:start(),
    update_config("config/grove.conf").
%    mnesia:wait_for_tables([shop], 20000).

test_tables() ->
    mnesia:clear_table(shop),
    mnesia:clear_table(cost),
    F = fun() ->
		lists:foreach(fun mnesia:write/1, test_data())
	end,
    mnesia:transaction(F).

mnesia_setup() ->
    test_schema(),
    test_start(),
    test_tables(),
    ok.

mnesia_cleanup(_result) ->
    mnesia:stop(),
    mnesia:delete_schema(node()),
    ok.

