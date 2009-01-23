-module(grove_custom).
-compile(export_all).

-define(MOD, grove_mnesia).

%%-----------------------------------------------------------------------------------------------
%%Function:    find/2
%%Description: finds the given object based on id
%%             TODO should be moved out to grove_custom or grove_rails
%%-----------------------------------------------------------------------------------------------
find(Object, [ID]) ->
    Result = ?MOD:run_query({parts, 
		   {table, Object}, 
		   {columns, all}, 
		   {operations, [?MOD:eq(?MOD:column(Object, item), ID)]}, 
		   {order, []}}),
    ?MOD:format_json(Result, Object, all).
