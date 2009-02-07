-module(grove).
-export([get_query/3,
	 post_query/2,
	 json_response/1,
	 string_response/1,
	 error_status/0,
         start/0,
	 update_config/0]).

-record(config_entry, {name, value}).

-define(ERROR_STATUS, {status, 400}).
-define(JSON_ROOT_ELEMENT, "{'result': ~s }").
-define(STRING_ROOT_ELEMENT, "{'result' : '~s' }").
-define(JSON_MIMETYPE, "application/json").
-define(OBJECT_NOT_AVAILABLE, " is not available for querying.").

-include_lib("stdlib/include/qlc.hrl"). 

%%-----------------------------------------------------------------------------------------------
%% Function:    get_query
%% @doc called from out/1 when a GET request is processed with /object/action uri
%%              and possibly /object/action/param1/param2/etc/. Compiles fun that calls
%%              the Function in the defined module with the Object and Params 
%% Arguments:   Object: should represent a table in the data store being queried.
%%              Action: MUST represent a function in the defined module
%%              Params: represents the rest of the URI path elements or an empty set
%%-----------------------------------------------------------------------------------------------
get_query(Obj, Act, Params) ->

    {CMod, DMod, PObjs} = read_config(),
    Function = list_to_atom(string:to_lower(Act)),
    Object = string:to_lower(Obj),
    {value, {exports, Exports}} = lists:keysearch(exports, 1, CMod:module_info()),

    %make sure the action is implemented, the object exists, and the object is permitted
    case {DMod:object_exists(Object), lists:member(Object, PObjs), lists:member({Function, 2}, Exports)} of
	{false, _, _} -> string_response(Object ++ ?OBJECT_NOT_AVAILABLE);
	{_, false, _} -> string_response(Object ++ ?OBJECT_NOT_AVAILABLE);
	{_, _, false} -> string_response(Function ++ " is not implemented.");
	{true, true, true} -> json_response(CMod:Function(Object, Params))
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
	{false, _ , _} -> string_response(Object ++ ?OBJECT_NOT_AVAILABLE);
	{_, false, _ } -> string_response(Object ++ ?OBJECT_NOT_AVAILABLE);
	{true, true, {struct,
		      [{"query",
			{array,
			 [{struct,[{"columns",Columns}]},
			  {struct,[{"operations", Ops}]},
			  {struct,[{"order", Ord}]}]}}]} } ->
	    Result = DMod:run_query({qry, 
				     {table, Object}, 
				     {columns, Columns}, 
				     {operations, Ops}, 
				     {order, Ord}}),
	    json_response(DMod:format_json(Result, Object, Columns));
	_other -> invalid_json() 
    end.

%%-----------------------------------------------------------------------------------------------
%% Function:    content
%% Description: pulls the content response together 
%%-----------------------------------------------------------------------------------------------
json_response(JSON) -> {content, ?JSON_MIMETYPE, io_lib:format( ?JSON_ROOT_ELEMENT, [JSON])}.

%%-----------------------------------------------------------------------------------------------
%% Function:    error_status
%% Description: moved to a function to facilitate something more complex than a simple tuple
%%              result down the road
%%-----------------------------------------------------------------------------------------------
string_response(JSON) ->  {content, ?JSON_MIMETYPE, io_lib:format( ?STRING_ROOT_ELEMENT, [JSON])}.

%%Generic error response for appmod/yapp/mochiweb use
error_status() -> ?ERROR_STATUS.

invalid_json() ->
     string_response("Invalid JSON String. Your JSON must take the format: " ++
				  "{\"query\" : " ++ 
				  "{\"columns\" : []}," ++ 
				  "{\"operations\" :  [] }," ++ 
				  "{\"order\": [] }] }").

start() ->
    mnesia:start(),
%% needs to check for errors in the yaws config file file
    _ = yaws:start(),
    update_config().


consult_config() ->
    {ok, Config} = file:consult("config/config.txt"),
    Config.


verify_config(Table) when is_atom(Table) ->
    Tables = mnesia:system_info(tables),
    case lists:member(Table, Tables) of
	true ->
	    ok;
	false -> 
	    mnesia:create_table(Table, [{attributes, record_info(fields, config_entry)}]),
	    ok	 
    end.


write_config(_table, []) -> ok;

write_config(Table, [{Name, Value}|T]) when is_atom(Table) ->
    Entry = {grove_config, Name, Value},
    mnesia:transaction(fun() -> mnesia:write(Entry) end),
    write_config(Table, T).
    

update_config() ->
    [{config_table_name, ConfTableName}|Config] = consult_config(),
    ok = verify_config(ConfTableName),
    ok = write_config(ConfTableName, Config).


query_config(EntryName) when is_atom(EntryName) ->
    T = fun() -> 
		qlc:e(qlc:q([ X || X <- mnesia:table(grove_config), X#config_entry.name =:= EntryName]))
	end,
    {atomic, [{grove_config, EntryName, Result}]} = mnesia:transaction(T),
    Result.

read_config() ->
    { query_config(custom_mod),
      query_config(data_adapter_mod),
      grove_util:all_lower_strings(query_config(objects)) }.
