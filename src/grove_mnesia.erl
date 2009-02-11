-module(grove_mnesia).
-export([object_exists/1, 
	 run_query/1,
	 format_json/3,
	 column/2
	]).

-define(NOTEST, 1).
-define(DEFAULT_QLC_LOCATION, code:lib_dir() ++ "/stdlib-1.15.5/include/qlc.hrl").
-define(TEMPORARY_MODULE, add_fun).
-define(TEMPORARY_FUNCTION, run_fun).

-include_lib("eunit/include/eunit.hrl").

%%
%%@doc This is the main function through which the grove module executes queries to 
%%mnesia. It uses the same standard query tuple, used by all other grove_* data
%%adapter modules
%%
run_query({qry, {table, Table}, {columns, _columns}, {operations, _ops}, {order, Ord}}) ->

    %format_query doesn't take the order, as it is applied externally via another qlc function
    Compr = format_query({qry, {table, Table}, {columns, _columns}, {operations, _ops}}),

    %Build the function string for smerl to compile
    FuncDef = query_func(Compr, Ord),

    %Build the code that will execute our query
    compile_query(?TEMPORARY_MODULE, FuncDef, Table),

    %Run the query and retrieve the result
    {atomic, Result} = ?TEMPORARY_MODULE:?TEMPORARY_FUNCTION(),

    %send back the result
    Result.

compile_query(ModName, FuncDef, Table) when is_atom(ModName)->
    AttrList = attribute_names(Table),
    Mod = smerl:new(ModName),
    {ok, RecAdded} = smerl:add_rec(Mod,record(Table, AttrList)),
    {ok, InclAdded} = smerl:add_incl(RecAdded, ?DEFAULT_QLC_LOCATION, qlc),
    {ok, FuncAdded} = smerl:add_func(InclAdded, FuncDef),
    ok = smerl:compile(FuncAdded).



%%----------------------------------------------------------------------------------------------------
%%Description: format_query deals with the qry tuple by placing the resulting values for 
%%each element into the same position in the tuple and passing it on. Finally it returns 
%%the comprehension string that constitutes the query
%%----------------------------------------------------------------------------------------------------
format_query({qry, {table, Name}, {columns, Columns}, Ops}) ->
    ColStr = format_columns(Name, Columns),
    format_query({qry, {table, Name}, ColStr, Ops});

format_query({qry, {table, Name}, Col, {operations, Ops}}) ->
    OpStr = format_operations(Name, Ops),
    format_query({qry, {table, Name}, Col, OpStr});

format_query({qry, {table, Name}, Col, Ops}) ->
    TableGen = format_generator(Name),
    format_query({qry, TableGen, Col, Ops});

%%!!must be last to finalize the formatted comprehension
format_query({qry, Table, Col, []}) ->
    grove_util:sfrmt("[ ~s || ~s ]",
		     [Col, Table]);

format_query({qry, Table, Col, Ops}) ->
    grove_util:sfrmt("[ ~s || ~s , ~s ]",
		     [Col, Table, Ops]).

%%----------------------------------------------------------------------------------------------------
%%Description: format_generator builds the generator for the comprehension. The variable is the table 
%%name as a standard. 
%%----------------------------------------------------------------------------------------------------
format_generator(Table) when is_list(Table) ->
    grove_util:sfrmt("~s <- mnesia:table(~s)",
		     [grove_util:initcap(Table),
		      string:to_lower(Table)]);

format_generator(Table) ->
    format_generator(grove_util:to_string(Table)).



%%----------------------------------------------------------------------------------------------------
%%Description: format operations handles the comparison operations in the comprehension for example
%%             code: eq(column(table, column1), 3.5) => Table#table.column1 =:= 3.5
%%             json: {"operations" :  [  {"eq" : {"column1" : "foo" }}]} => Table#table.column1 =:= "foo"
%%          
%%             !Its important to note that if the "foo" in the JSON example were changed to a string that 
%%             matches a column in the table being queried it will transform it into column syntax
%%             to allow column to column comparisons within a single table
%%            
%%             if you want to force a string/atom use the following ("string" <=> "atom"):
%%             {"operations" :  [  {"eq" : {"column1" : { "string" : "column2" } }}]} =>
%%             Table#table.column1 =:= "column2"  
%%             instead of
%%             Table#table.column1 =:= Table#table.column2
%%----------------------------------------------------------------------------------------------------
format_operations(Table, none) ->
    format_operations(Table, []);

format_operations(Table, {array, []}) ->
    format_operations(Table, []);

format_operations(_table, []) -> [];

format_operations(Table, {array, [{struct, _operation}|_t] = OpList}) ->
    format_operations(Table, OpList, []);

%%handles direct string from grove_custom or other custom get query modules
format_operations(_table, OpStrList) when is_list(OpStrList) ->
    OpStrList.

format_operations(_table, [], Ops) ->
    string:join(Ops, ", ");

format_operations(Table, [{struct, [{Op, {struct, [{LOp, ROp}]}}]}|T], Ops) ->
    Operation = list_to_atom(grove_util:to_string(Op)),
    OpStr = grove_mnesia_ops:Operation(format_operand(Table, LOp), format_operand(Table, ROp)),
    format_operations(Table, T, Ops ++ [OpStr]).

%%----------------------------------------------------------------------------------------------------
%%Description: Returns the columns list string necessary for limiting those columns in an mnesia
%%             query string, the atom all produces "Table ||" 
%%----------------------------------------------------------------------------------------------------

format_columns(Table, []) ->
    format_columns(Table, all);

format_columns(Table, "all") ->
    format_columns(Table, all); 

format_columns(Table, {array, []}) ->
    format_columns(Table, all);

format_columns(Table, all) ->
    grove_util:initcap(Table);

format_columns(Table, {array, Columns}) when is_list(Columns) ->
    format_columns(Table, Columns, []);

format_columns(Table, Columns) when is_list(Columns) ->
    format_columns(Table, Columns, []).


%%-----------------------------------------------------------------------------------------------
%%Description: creates the correct string for selecting specific columns in a set comprehension 
%%-----------------------------------------------------------------------------------------------
format_columns(Table, [], ColumnList) ->
    grove_util:sfrmt("{ ~s , ~s }", [grove_util:to_lower_string(Table), string:join(ColumnList, ", ")]);

format_columns(Table, [Column|T], ColumnList) ->
    format_columns(Table, T, ColumnList ++ [column(Table, Column)]).

%%-----------------------------------------------------------------------------------------------
%%Description: returns the string representing a column in an mnesia query, built from the 
%%             Record and Field name. 
%%-----------------------------------------------------------------------------------------------
column(Table, Name) ->
    Tstr = grove_util:to_string(Table),
    Nstr = grove_util:to_string(Name),
    grove_util:sfrmt("~s#~s.~s",
		     [grove_util:initcap(Tstr), 
		      string:to_lower(Tstr), 
		      string:to_lower(Nstr)]).
%%-----------------------------------------------------------------------------------------------
%%Description: Formats a mnesia query result in to a json array of objects. for example the table 
%%             defined by the record
%%
%%             -record({shop, {item, quantity, cost}) 
%%
%%             would have a JSON result something like
%%              
%%             [{"cost":2.3,"quantity":20,"item":"apple"}, {"cost":2.0,"quantity":10,"item":"orange"}]
%%        
%%             **Currently it only works with key/value pairs, where value is a string or 
%%             a number. 
%%-----------------------------------------------------------------------------------------------
format_json(Rows, Table, []) ->
    format_json(Rows, Table, all);

format_json(Rows, Table, "all") ->
    format_json(Rows, Table, all);

format_json(Rows, Table, {array, []}) ->
    format_json(Rows, Table, all);

format_json(Rows, Table, all) when is_list(Rows), is_list(Table) ->
    Attributes = attribute_names(Table),
    json_array(Attributes, Rows);

format_json(Rows, Table, {array, Columns}) when is_list(Rows), is_list(Table) ->
    Attributes = grove_util:intersection(attribute_names(Table), grove_util:all_lower_strings(Columns)),
    json_array(Attributes, Rows).


json_array(A, R) ->
    json_array(A, R, []).

json_array(_, [] ,JSONArray) ->
    Utf8 = mochijson:encoder([{input_encoding, utf8}]),
    lists:flatten(Utf8({array, JSONArray}));

json_array(Attributes, [Row|T], JSONArray) when is_tuple(Row)->
    json_array(Attributes, T, JSONArray ++ [format_result_struct(Attributes, tuple_to_list(Row))]).

%%-----------------------------------------------------------------------------------------------
%%Description: Turns a row set result from mnesia into part of the result struct that mochijson2
%%             can format. Head of the initial call is removed because it is assumed to be the 
%%             atom of the table name, from queries with/without column definitions
%%-----------------------------------------------------------------------------------------------

%%removes the atom representing the table name from the single row of the result
format_result_struct(Attributes, [_h|Values]) when is_list(Attributes) ->
    format_result_struct(Attributes, Values, {struct, []}).

%%return the structure to be encoded 
format_result_struct([], [], Struct) -> Struct;

%%converts binary, kept here from mochijson2 comapatability, and for future compatability
format_result_struct([Attr|T], Vals, {struct, KeyValueList}) when is_binary(Attr)->
    format_result_struct([binary_to_list(Attr)|T], Vals, {struct, KeyValueList});

%%put the column names and their values together
format_result_struct([Attr|T], [Val|T2], {struct, KeyValueList}) ->
    format_result_struct(T, T2, {struct, KeyValueList ++ [{Attr, Val}]}).

%%TODO either alter mochijson to handle abitrarily complex values from mnesia or handle it here
attribute_names(Table) when is_list(Table) ->
    attribute_names(list_to_atom(Table));

attribute_names(Table) when is_atom(Table) ->
    Attributes = mnesia:table_info(Table,  attributes),
    grove_util:all_lower_strings(Attributes).
		

record(_table, []) ->
    throw(invalid_record_attributes);

record(Table, AttrList) when is_atom(Table), is_list(AttrList) ->
    record(atom_to_list(Table), AttrList);

record(Table, AttrList) when is_list(AttrList)->
    grove_util:sfrmt("-record( ~s , { ~s }).", 
		     [string:to_lower(Table), 
		      string:join(AttrList, ", ")]).

%%-----------------------------------------------------------------------------------------------
%%Description: returns the function code to execute the list comprehension query, without order
%%             hard not to feel like this is really hackish. Need to implement sfrmt in 
%%             grove util. 
%%-----------------------------------------------------------------------------------------------

query_func(Comprehension, []) ->
    query_func(Comprehension, {array, []});

query_func(Comprehension, {array , []}) ->
    grove_util:sfrmt("~s() -> mnesia:transaction(fun() ->qlc:e( qlc:q( ~s ) ) end).",
		     [?TEMPORARY_FUNCTION, 
		      Comprehension]);

query_func(Comprehension, {array, [Ord]}) ->
    query_func(Comprehension, Ord);

query_func(Comprehension, Ord) ->
    Order = case grove_util:to_lower_string(Ord) of
		"descending" ->  Ord;
		"ascending" -> Ord;
		_other -> throw(invalid_order_argument)
	    end,

    grove_util:sfrmt("~s() -> mnesia:transaction(fun() ->qlc:e( qlc:sort( qlc:q( ~s ), {order, ~s } ) ) end).",
		     [?TEMPORARY_FUNCTION, 
		      Comprehension, 
		      Order]).

%%-----------------------------------------------------------------------------------------------
%%Description: Checks to see if the table is present in mnesia
%%-----------------------------------------------------------------------------------------------
object_exists(Table) when is_list(Table) ->
    object_exists(list_to_atom(string:to_lower(Table)));

object_exists(Table) when is_atom(Table) ->
    try lists:member(Table, mnesia:system_info(tables))
    catch 
        exit : _reason -> false
    end.


format_operand(_table, Op) when is_number(Op) -> Op;

format_operand(_table, {struct , [{"string", Op}]}) ->
    "\""++ grove_util:to_string(Op) ++ "\"";

format_operand(_table, {struct , [{"atom", Op}]}) ->
    grove_util:to_lower_string(Op);

format_operand(Table, Op) ->
    Operand = grove_util:to_string(Op),
    Attr = attribute_names(Table),
    case lists:member(Operand, Attr) of
	true -> column(Table, Operand);
	false -> "\"" ++ Operand ++ "\""
    end.


%--------UNIT TESTS---------

format_query_test() ->
    "[ Foo || Foo <- mnesia:table(foo) ]" = format_query({qry, {table, foo}, {columns, all}, {operations, []}}),
    ?assertException(error, function_clause, format_query({qry, {table, foo}, {columns, all}, {operations, []}, {order, []}})).

format_generator_test() ->
    "Foo <- mnesia:table(foo)" = format_generator(foo).
    

format_operations_test() ->
    "Table#table.column == 3.2" = format_operations('Table', grove_mnesia_ops:eq(column(table, column), 3.2)),
    "Table#table.column == 3.2" = format_operations(table, grove_mnesia_ops:eq(column(table, column), 3.2)),
    "Table#table.column == 3.2" = format_operations("Table", grove_mnesia_ops:eq(column(table, column), 3.2)),
    "Table#table.column == Table#table.column2" = format_operations(table, grove_mnesia_ops:eq(column(table, column), column(table, column2))).

format_columns_test() ->
    "Table" = format_columns(table, all),
    "{ table , Table#table.column1, Table#table.column2 }" = format_columns('Table', {array, [column1, column2]}),
    "{ table , Table#table.column1, Table#table.column2 }" = format_columns(table, [column1, column2]).


column_test() ->
    "Table#table.column" = column("Table", column),
    "Table#table.column" = column("Table", <<"column">>),
    "Table#table.column" = column("Table", "column").

json_array_test() ->
    "[{\"firstattr\":\"test\"}]" = json_array( [ "firstattr" ], [{table, "test"}]),
    "[{\"firstattr\":\"test\"}]" = json_array( [ firstattr ], [{table, test}]),
    "[{\"firstattr\":3.2}]" = json_array( [ "firstattr" ], [{table, 3.2}]),
    ?assertException(error, function_clause, json_array( [ "firstattr", test ], [{table, 3.2}])).


format_result_struct_test() ->
    {struct, [{"test", test_value}]} = format_result_struct(["test"], [table, test_value]),
    {struct, [{"test", "test_value"}]} = format_result_struct(["test"], [table, "test_value"]),
    {struct, []} = format_result_struct([], [], {struct, []}),
    {struct, [{"test", "test_value"}]} = format_result_struct(["test"], ["test_value"], {struct, []}),
    {struct, [{"test", test_value}]} = format_result_struct(["test"], [test_value], {struct, []}),
    {struct, [{test, test_value}]} = format_result_struct([test], [test_value], {struct, []}),
    {struct, [{"test", test_value}, {test2, test_value2}]} = format_result_struct(["test", test2], [test_value, test_value2], {struct, []}),
    ?assertException(error, function_clause, format_result_struct([], [])).

%integration test
attribute_names_test() ->
    ?assertException(exit, _, attribute_names(arbitrary_non_existant_table)).

record_test() ->
    "-record( test_record , { attribute1 })."= record(test_record, ["attribute1"]),
    "-record( test_record , { attribute1, attribute2 })."= record("test_record", ["attribute1", "attribute2"]),
    ?assertException(throw, invalid_record_attributes, record(table, [])).

object_exists_test() ->
    false = object_exists(arbitrary_non_existant_table), 
    false = object_exists("arbitrary_string_table"),
    ?assertException(error, function_clause, object_exists(2)).

format_operand_test() ->
    "\"test\"" = format_operand(table, {struct , [{"string", "test"}]}),
    "test" = format_operand(table, {struct , [{"atom", "test"}]}),
    2.3 = format_operand(table, 2.3).
    %need to add test for creating column

query_func_test()->
    Test = "TEST",
    "run_fun() -> mnesia:transaction(fun() ->qlc:e( qlc:q( TEST ) ) end)." =  query_func(Test, []),
    "run_fun() -> mnesia:transaction(fun() ->qlc:e( qlc:sort( qlc:q( TEST ), {order, ascending } ) ) end)." =  query_func(Test, "ascending"),
    "run_fun() -> mnesia:transaction(fun() ->qlc:e( qlc:sort( qlc:q( TEST ), {order, descending } ) ) end)." =  query_func(Test, {array, ["descending"]}),
    ?assertException(throw, invalid_order_argument, query_func(Test, ["descend"])).
    


