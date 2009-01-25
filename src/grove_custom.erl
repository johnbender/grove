-module(grove_custom).
-compile(export_all).
-define(ADAPT, grove_mnesia).
-define(OPS, grove_mnesia_ops).

%%-----------------------------------------------------------------------------------------------
%%Function:    find/2
%%Description: finds the given object based on id
%%             TODO should be moved out to grove_custom or grove_rails
%%-----------------------------------------------------------------------------------------------
find(Object, [ID]) ->
    Result = ?ADAPT:run_query({parts, 
		   {table, Object}, 
		   {columns, all}, 
		   {operations, [?OPS:eq(?ADAPT:column(Object, item), ID)]}, 
		   {order, []}}),
    ?ADAPT:format_json(Result, Object, all).
