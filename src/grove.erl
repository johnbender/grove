-module(grove).
-export([get_query/3,
	 post_query/2,
	 json_response/1,
	 string_response/1,
	 error_status/0]).

%%-----------------------------------------------------------------------------------------------
%% FUNCTION defines the format for the smerl compiled function to call for the requested
%%                 action on a given object for given params /Object/Action/Param1/Param2/
%% JSON_ROOT_ELEMENT defines how the JSON response will look
%% JSON_MIMETYPE defines the mime string for the response
%% ERROR_STATUS passed back to the client when something isn't right
%%-----------------------------------------------------------------------------------------------
-define(ERROR_STATUS, {status, 400}).
-define(JSON_ROOT_ELEMENT, "{'result': ~s }").
-define(STRING_ROOT_ELEMENT, "{'result' : '~s' }").
-define(JSON_MIMETYPE, "application/json").
-define(OBJECT_NOT_AVAILABLE, " is not available for querying.").



%%-----------------------------------------------------------------------------------------------
%% Function:    get_query
%% Description: called from out/1 when a GET request is processed with /object/action uri
%%              and possibly /object/action/param1/param2/etc/. Compiles fun that calls
%%              the Function in the defined module with the Object and Params 
%% Arguments:   Object: should represent a table in the data store being queried.
%%              Action: MUST represent a function in the defined module
%%              Params: represents the rest of the URI path elements or an empty set
%%-----------------------------------------------------------------------------------------------
get_query(Object, Action, Params ) ->

    %%TEMPORARY, todo get from config file
    Module = list_to_atom("grove_mnesia"),
    %%------------------------------------
    
    Function = list_to_atom(Action),
    {value, {exports, Exports}} = lists:keysearch(exports, 1, Module:module_info()),
						
    %make sure the action is implemented, and the object exists
    case {Module:object_exists(Object), lists:member({Function, 2}, Exports)} of
	{false, _} -> string_response(Object ++ ?OBJECT_NOT_AVAILABLE);
	{_, false} -> string_response(Action ++ " is not implemented.");
	{true, true} -> json_response(Module:Function(Object, Params))
    end.

%%-----------------------------------------------------------------------------------------------
%% Function:    post_query
%% Description: called from out/1 when a POST request is processed with /object URI
%%-----------------------------------------------------------------------------------------------
post_query(Object, JSON) -> 

    %%TEMPORARY, todo get from config file
    Module = list_to_atom("grove_mnesia"),
    
    case {Module:object_exists(Object), mochijson2:decode(JSON)} of
	{false, _} -> string_response(Object ++ ?OBJECT_NOT_AVAILABLE);
	{true, {struct,[{<<"query">>,
			 [{struct,[{<<"columns">>,Columns}]},
			  {struct,[{<<"operations">>, Ops}]},
			  {struct,[{<<"order">>, Ord}]}]}]} } ->
	    Result = Module:run_query({parts, 
					    {table, Object}, 
					    {columns, Columns}, 
					    {operations, Ops}, 
					    {order, Ord}}),
	    json_response(Module:format_json(Result, Object, Columns));
	{true, _other} -> string_response("Your JSON must take the format: " ++
				  "{\"query\" : " ++ 
				  "{\"columns\" : []}," ++ 
				  "{\"operations\" :  [] }," ++ 
				  "{\"order\": [] }] }")
    end.



%%direct_query(Object, Action, Params)
%% for direct integration with rails as a backend/data services (do authentication necessary actions through rails)

%%set_response(Set)
%% return a tuple {set, Set} so that rails can turn them into objects

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
