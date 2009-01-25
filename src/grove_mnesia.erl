-module(grove_mnesia).

-export([object_exists/1, 
	 run_query/1, 
	 run_query/3,
	 format_json/3,
	 column/2
	]).

-define(NOTEST, 1).
-define(DEFAULT_QLC_LOCATION, "/opt/erlang/lib/erlang/lib/stdlib-1.15.5/include/qlc.hrl").
-define(TEMPORARY_MODULE, add_fun).
-define(TEMPORARY_FUNCTION, run_fun).

-include_lib("eunit/include/eunit.hrl").

run_query({parts, {table, Table}, {columns, Columns}, {operations, Ops}, {order, Ord}}) ->
    Compr = format_query({parts, {table, Table}, {columns, Columns}, {operations, Ops}}),
    run_query(Compr, Table, Ord).

run_query(Query, Table, Ord) when is_list(Query) ->
    FuncDef = query_func(Query, Ord),
    io:format("~s", [FuncDef]),
    compile_query(?TEMPORARY_MODULE, FuncDef, Table),
    {atomic, Result} = ?TEMPORARY_MODULE:?TEMPORARY_FUNCTION(),
    Result.

compile_query(ModName, FuncDef, Table) when is_atom(ModName)->
    AttrList = attribute_names(Table),
    Mod = smerl:new(ModName),
    {ok, RecAdded} = smerl:add_rec(Mod,record(Table, AttrList)),
    {ok, InclAdded} = smerl:add_incl(RecAdded, ?DEFAULT_QLC_LOCATION, qlc),
    {ok, FuncAdded} = smerl:add_func(InclAdded, FuncDef),
    ok = smerl:compile(FuncAdded).

%%-----------------------------------------------------------------------------------------------
%%Function:    format_query/2
%%Description: deals with the query Parts tuple by placing the resulting values for each element
%%             into the same position in the tuple and passing it on. Finally it returns the 
%%             set comprehension string that constitutes the query from the Parts specified
%%-----------------------------------------------------------------------------------------------

%%an empty set or the atom all in conjunction with the columns produce the same result		    
format_query({parts, {table, Name}, {columns, Columns}, Ops}) ->
    ColStr = format_columns(Name, Columns),
    format_query({parts, {table, Name}, ColStr, Ops});

format_query({parts, {table, Name}, Col, {operations, Ops}}) ->
    OpStr = format_operations(Name, Ops),
    format_query({parts, {table, Name}, Col, OpStr});

format_query({parts, {table, Name}, Col, Ops}) ->
    TableGen = format_generator(Name),
    format_query({parts, TableGen, Col, Ops});

%%!!must be last to finalize the formatted comprehension
format_query({parts, Table, Col, Ops}) -> 
    grove_util:string_format("[ ~s , ~s, ~s ]",
			     [Col,
			      Table,
			      Ops]).

%%-----------------------------------------------------------------------------------------------
%%Function:    format_generator/2
%%Description: builds the generator for the comprehension. The variable is the table name as a 
%%             standard. 
%%-----------------------------------------------------------------------------------------------
format_generator(Table) ->
    grove_util:string_format("~s <- mnesia:table(~s) ",
			     [grove_util:initcap(Table),
			      string:to_lower(Table)]).

%%-----------------------------------------------------------------------------------------------
%%Function:    format_operations/2
%%Description: format operations handles the comparison operations in the comprehension for example
%%             code: eq(column(table, column1), 3.5) => Table#table.column1 =:= 3.5
%%             json: {"operations" :  [  {"eq" : {"column1" : 2.3 }}]} => Table#table.column1 =:= 2.3
%%             its important to note that if the 2.3 in the JSON example were changed to a string that 
%%             matches a column in the table being queried it will transform it into column syntax
%%             to allow column to column comparisons within a single table
%%            
%%             if you want to force a string use the following:
%%             {"operations" :  [  {"eq" : {"column1" : { "string" : "column2" } }}]} =>
%%             Table#table.column1 =:= "column2" 
%%-----------------------------------------------------------------------------------------------
format_operations(Table, none) ->
    format_operations(Table, []);
format_operations(_table, []) -> [];

format_operations(Table, [{struct, _operation}|_t] = OpList) ->
    format_operations(Table, OpList, []);
format_operations(Table, OpStrList) when is_list(OpStrList) ->
    format_operations(Table, [], OpStrList).

format_operations(_table, [], Ops) ->
    ", " ++ string:join(Ops, ", ");
format_operations(Table, [{struct, [{Op, {struct, [{LOp, ROp}]}}]}|T], Ops) ->
    Operation = list_to_atom(grove_util:to_string(Op)),
    OpStr = grove_mnesia_ops:Operation(format_operand(Table, LOp), format_operand(Table, ROp)),
    format_operations(Table, T, [OpStr|Ops]).

%%-----------------------------------------------------------------------------------------------
%%Function:    format_columns/2
%%Description: Returns the columns list string necessary for limiting those columns in an mnesia
%%             query string, the atom all produces "Table ||" 
%%-----------------------------------------------------------------------------------------------
format_columns(Table, []) ->
    format_columns(Table, all);
format_columns(Table, <<"all">>) ->
    format_columns(Table, all); 
format_columns(Table, all) ->
    grove_util:initcap(Table) ++ " || ";
format_columns(Table, Columns) when is_list(Columns) ->
    format_columns(Table, Columns, "").

%%-----------------------------------------------------------------------------------------------
%%Function:    format_columns/3
%%Description: creates the correct string for selecting specific columns in a set comprehension 
%%-----------------------------------------------------------------------------------------------
format_columns(Table, [], ColumnList) ->
    grove_util:string_format("{ ~s , ~s } || ", 
			     [string:to_lower(Table),
			      string:join(ColumnList, ", ")]);
format_columns(Table, [<<Column>>|T], ColumnList) ->
    format_columns(Table, [Column|T], ColumnList);
format_columns(Table, [Column|T], ColumnList) ->
    format_columns(Table, 
		   T, 
		   [column(Table, Column)|ColumnList]).

%%-----------------------------------------------------------------------------------------------
%%Function:    column/2
%%Description: returns the string representing a column in an mnesia query, built from the 
%%             Record and Field name. 
%%-----------------------------------------------------------------------------------------------
column(Table, Name) when is_binary(Name) ->
    column(Table, binary_to_list(Name));
column(Table, Name) when is_atom(Name) -> 
    column(Table, atom_to_list(Name));
column(Table, Name) ->
    grove_util:string_format("~s#~s.~s",
			     [grove_util:initcap(Table), 
			      string:to_lower(Table), 
			      string:to_lower(Name)]).
%%-----------------------------------------------------------------------------------------------
%%Function:    format_json/2
%%Description: Formats a mnesia query result in to a json array of objects. for example the table 
%%             defined by the record
%%
%%             -record({shop, {item, quantity, cost}) 
%%
%%             would have a JSON result something like
%%              
%%             [{"cost":2.3,"quantity":20,"item":"apple"}, {"cost":2.0,"quantity":10,"item":"orange"}]
%%        
%%             **Currently its only been tested with key/value pairs, where value is a string or 
%%             a number. 
%%-----------------------------------------------------------------------------------------------
format_json(Rows, Table, []) ->
    format_json(Rows, Table, all);
format_json(Rows, Table, <<"all">>) ->
    format_json(Rows, Table, all);
format_json(Rows, Table, all) when is_list(Rows), is_list(Table) ->
    Attributes = attribute_names(Table),
    json_tuple(Rows, Attributes, []);
format_json(Rows, Table, Columns) when is_list(Rows), is_list(Table) ->
    Attributes = grove_util:intersection(attribute_names(Table), grove_util:all_lower_strings(Columns)),
    json_tuple(Rows, Attributes, []).

json_tuple([], _, JSONArray) ->
    mochijson2:encode(JSONArray);

json_tuple([Row|T], Attributes, JSONArray) when is_tuple(Row)->
    json_tuple(T, Attributes, [format_struct(Attributes, tuple_to_list(Row))|JSONArray]).

%%-----------------------------------------------------------------------------------------------
%%Function:    format_struct/2
%%Description: Turns a row set result from mnesia into part of the result struct that mochijson2
%%             can format
%%-----------------------------------------------------------------------------------------------
format_struct(Attributes,[_h|Values])when is_list(Values), is_list(Attributes) ->
    format_struct(Attributes, Values, {struct, []}).

format_struct([], [], Struct) -> Struct;

format_struct([Attr|T], [Val|T2], {struct, KeyValueList}) when is_binary(Attr)->
    format_struct(T, T2, {struct, [{string:to_lower(binary_to_list(Attr)), Val}|KeyValueList]});

format_struct([Attr|T], [Val|T2], {struct, KeyValueList}) ->
    format_struct(T, T2, {struct, [{Attr, Val}|KeyValueList]}).

%%TODO either alter mochijson to handle abitrarily complex values from mnesia or handle it here
%%probably a doosy
attribute_names(Table) when is_list(Table) ->
    attribute_names(list_to_atom(Table));

attribute_names(Table) when is_atom(Table) ->
    Attributes = mnesia:table_info(Table,  attributes),
    grove_util:all_lower_strings(Attributes).
		
record(Table, AttrList) when is_atom(Table) ->
    record(atom_to_list(Table), AttrList);
record(Table, AttrList) when is_list(AttrList) ->
    grove_util:string_format("-record( ~s , { ~s }).", 
			     [string:to_lower(Table), 
			      string:join(AttrList, ", ")]).

%%-----------------------------------------------------------------------------------------------
%%Function:    query_func/2
%%Description: returns the function code to execute the list comprehension query, without order
%%             hard not to feel like this is really hackish. Need to implement string_format in 
%%             grove util. 
%%-----------------------------------------------------------------------------------------------
query_func(Comprehension, []) ->
    grove_util:string_format("~s() -> mnesia:transaction(fun() ->qlc:e( qlc:q( ~s ) ) end).",
			     [grove_util:to_string(?TEMPORARY_FUNCTION), 
			      Comprehension]);
query_func(Comprehension, Ord) ->
    grove_util:string_format("~s() -> mnesia:transaction(fun() ->qlc:e( qlc:sort( qlc:q( ~s ), {order, ~s } ) ) end).",
			     [grove_util:to_string(?TEMPORARY_FUNCTION), 
			      Comprehension, 
			      grove_util:to_string(Ord)]).


%%-----------------------------------------------------------------------------------------------
%%Function:    object_exists/1
%%Description: Checks to see if the table is present in mnesia
%%-----------------------------------------------------------------------------------------------
object_exists(Table) when is_list(Table) ->
    object_exists(list_to_atom(string:to_lower(Table)));
object_exists(Table) when is_atom(Table) ->
    try lists:member(Table, mnesia:system_info(tables))
    catch 
        exit : _reason -> false
    end.

format_operand(_table, {struct , [{Type, Op}]}) ->
    case Type of
	<<"string">> -> "\""++ grove_util:to_string(Op) ++ "\"";
	<<"atom">> -> list_to_atom(grove_util:to_string(Op))
    end;
format_operand(_table, Op) when is_number(Op) -> Op;
format_operand(Table, Op) ->
    Operand = grove_util:to_string(Op),
    Attr = attribute_names(Table),
    case lists:member(string:to_lower(Operand), Attr) of
	true -> column(Table, Operand);
	false -> "\"" ++ Operand ++ "\""
    end.


%--------TESTS---------


