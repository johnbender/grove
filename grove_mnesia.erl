-module(grove_mnesia).
-compile(export_all).
%-behaviour(grove_behaviour).

-define(OPERATION, "~s ~s ~s").
%%may not restrict ( or ) for grove_mysql because of in statement, we will see
-define(OPERAND_RESTRICT_REGEX, "[\|,;\(\)]|(\.\s)").
-define(DEFAULT_QLC_LOCATION, "/opt/erlang/lib/erlang/lib/stdlib-1.15.5/include/qlc.hrl").
-define(TEMPORARY_MODULE, add_fun).
-define(TEMPORARY_FUNCTION, run_fun).

%%-----------------------------------------------------------------------------------------------
%%Function:    lt
%%Description: returns the string of a qualifier for a check of < for the left and right 
%%             arguments
%%-----------------------------------------------------------------------------------------------
lt([Left, Right]) ->
    lt(Left, Right).
lt(Left, Right) ->
    format_operation("~s < ~s", [Left, Right]).

%%-----------------------------------------------------------------------------------------------
%%Function:    lte
%%Description: returns the string of a qualifier for a check of <= for the left and right 
%%             arguments
%%-----------------------------------------------------------------------------------------------
lte([Left, Right]) -> 
    lte(Left, Right).
lte(Left, Right) -> 
    format_operation("~s <= ~s", [Left, Right]).

%%-----------------------------------------------------------------------------------------------
%%Function:    gt
%%Description: returns the string of a qualifier for a check of > for the left and right 
%%             arguments
%%-----------------------------------------------------------------------------------------------
gt([Left, Right]) -> 
    gt(Left, Right).
gt(Left, Right) -> 
    format_operation("~s > ~s", [Left, Right]).

%%-----------------------------------------------------------------------------------------------
%%Function:    gte
%%Description: returns the string of a qualifier for a check of >= for the left and right 
%%             arguments
%%-----------------------------------------------------------------------------------------------
gte([Left, Right]) -> 
    gte(Left, Right).
gte(Left, Right) -> 
    format_operation("~s >= ~s", [Left, Right]).

%%-----------------------------------------------------------------------------------------------
%%Function:    eq
%%Description: returns the string of a qualifier for a check of == for the left and right 
%%             arguments
%%-----------------------------------------------------------------------------------------------
eq([Left, Right]) -> 
    eq(Left, Right).
eq(Left, Right) -> 
    format_operation("~s =:= ~s", [Left, Right]).

%%-----------------------------------------------------------------------------------------------
%%Function:    neq
%%Description: returns the string of a qualifier for a check of =/= for the left and right 
%%             arguments
%%-----------------------------------------------------------------------------------------------
neq([Left, Right]) ->
    neq(Left, Right).
neq(Left, Right) ->
    format_operation("~s =/= ~s", [Left, Right]).

%%-----------------------------------------------------------------------------------------------
%%Function:    format_operation
%%Description: fits the operands into the operation format string
%%-----------------------------------------------------------------------------------------------
format_operation(Operation, Operands) ->
    ok = validate_operands(Operands),
    lists:flatten(io_lib:format(Operation, all_strings(Operands))).

all_strings(Objects) ->
    all_strings(Objects, []).

all_strings([], Result) ->
    Result;
all_strings([Object|T], Result) when is_number(Object) ->
    all_strings(T, [Result|io_lib:format("~w", [Object])]);
all_strings([Object|T], Result) when is_binary(Object) ->
    all_strings(T, [Result|binary_to_list(Object)]);
all_strings([Object|T], Result) when is_list(Object) ->
    all_strings(T, [Result|Object]).

%%-----------------------------------------------------------------------------------------------
%%Function:    validate_operands
%%Description: validates each of the operands agains the defined regex. Mnesia will allow 
%%             possible less than mysql (ie " in (value1, value2) " )
%%-----------------------------------------------------------------------------------------------
validate_operands([]) -> ok;
validate_operands([Operand|T]) when is_number(Operand) ->
    validate_operands(T);
validate_operands([Operand|T]) -> 
    case regexp:match(Operand, ?OPERAND_RESTRICT_REGEX) of
	{match, First, Second} -> throw({invalid_operand_characters, Operand, First, Second});
	nomatch -> validate_operands(T)
    end.

%%-----------------------------------------------------------------------------------------------
%%Function:    find/2
%%Description: finds the given object based on id
%%             TODO should be moved out to grove_custom or grove_rails
%%-----------------------------------------------------------------------------------------------
find(Object, [ID]) ->
    Result = run_query({parts, 
		   {table, Object}, 
		   {columns, all}, 
		   {operations, [eq(column(Object, item), ID)]}, 
		   {order, []}}),
    format_json(Result, Object, all).

run_query({parts, {table, Table}, {columns, _columns}, {operations, _ops}, {order, _ord}} = Query) ->
    Compr = format_query(Query),
    run_query(Compr, Table).

run_query(Query, Table) when is_list(Query) ->
    FuncDef = query_func(Query),
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
format_query({parts, {table, Name}, {columns, Columns}, Op, Ord}) ->
    ColStr = format_columns(Name, Columns),
    format_query({parts, {table, Name}, ColStr, Op, Ord});

format_query({parts, {table, Name}, Col, {operations, Ops}, Ord}) ->
    OpStr = format_operations(Name, Ops),
    format_query({parts, {table, Name}, Col, OpStr, Ord});

format_query({parts, {table, Name}, Col, Op, Ord}) ->
    TableGen = format_generator(Name),
    format_query({parts, TableGen, Col, Op, Ord});

format_query({parts, Table, Col, Op, {order, Ord}}) ->
    format_query({parts, Table, Col, Op, ""});

%%!!must be last to finalize the formatted query, the ordering might take place
format_query({parts, Table, Col, Op, Ord}) -> "[" ++ string:join([Col, Table, Op], " ") ++ "]".
    

format_generator(Table) ->
    initcap(Table) ++ " <- mnesia:table(" ++ string:to_lower(Table) ++ ")".

format_operations(Table, []) ->
   format_operations(Table, none);
format_operations(Table, none) ->
    " ";
format_operations(Table, [{struct, Operation}|T] = OpList) ->
    format_operations(Table, OpList, []);
format_operations(Table, OpStrList) when is_list(OpStrList) ->
    ", " ++ string:join(OpStrList, ", ").

format_operations(Table, [], Ops) ->
    string:join(Ops, ", ");
%                        [{struct, [{<<"eq">>, {struct, [{<<"cost">>, 2.3}]}}]}]
format_operations(Table, [{struct, [{Op  , {struct, [{LOp       , ROp}]}}]}|T], Ops) ->
    Operation = list_to_atom(binary_to_list(Op)),
    OpStr = ?MODULE:Operation(format_operand(Table, LOp), format_operand(Table, ROp)),
    format_operations(Table, T, [Ops|[OpStr]]).

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
    initcap(Table) ++ " || ";
format_columns(Table, Columns) when is_list(Columns) ->
    format_columns(Table, Columns, "").

%%-----------------------------------------------------------------------------------------------
%%Function:    format_columns/3
%%Description: creates the correct string for selecting specific columns in a set comprehension 
%%-----------------------------------------------------------------------------------------------

format_columns(_table, [], ColumnList) ->
    "{" ++ string:join(ColumnList, ", ") ++ "} ||";
format_columns(Table, [<<Column>>|T], ColumnList) ->
    format_columns(Table, [Column|T], ColumnList);
format_columns(Table, [Column|T], ColumnList) ->
    format_columns(Table, 
		   T, 
		   [column(Table, Column)|ColumnList]).

%%-----------------------------------------------------------------------------------------------
%%Function:    column/2
%%Description: returns the string representing a column in an mnesia query, built from the 
%%             Record and Field name
%%-----------------------------------------------------------------------------------------------
column(Table, Name) when is_binary(Name) ->
    column(Table, binary_to_list(Name));
column(Table, Name) when is_atom(Name) -> 
    column(Table, atom_to_list(Name));
column(Table, Name) ->
    initcap(Table) ++ "#" ++ string:to_lower(Table) ++ "." ++ string:to_lower(Name).

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
    io:format("~p", [Rows, Table, all]),
    Attributes = attribute_names(Table),
    format_json(Rows, Attributes, []);
format_json(Rows, Table, Columns) when is_list(Rows), is_list(Table) ->
    io:format("~p", [Rows]),
    format_json(Rows, Table, Columns, []).

format_json([], _, _, JSONArray) ->
    mochijson2:encode(JSONArray);

format_json([Row|T], _ ,Attributes, JSONArray) when is_tuple(Row)->

    io:format("~p", [Row]),
    format_json(T, Attributes, [format_struct(Attributes, tuple_to_list(Row))|JSONArray]).

%%-----------------------------------------------------------------------------------------------
%%Function:    format_struct/2
%%Description: Turns a row set result from mnesia into part of the result struct that mochijson2
%%             can format
%%-----------------------------------------------------------------------------------------------
format_struct(Attributes, [_table|Values])when is_list(Values), is_list(Attributes) ->
    
    io:format("~p", [Values]),
    format_struct(Attributes, Values, {struct, []}).

format_struct([], [], Struct) -> Struct;

format_struct([Attr|T], [Val|T2], {struct, KeyValueList}) when is_binary(Attr)->
    format_struct(T, T2, {struct, [{string:to_lower(binary_to_list(Attr)), Val}|KeyValueList]});

format_struct([Attr|T], [Val|T2], {struct, KeyValueList}) ->
    format_struct(T, T2, {struct, [{Attr, Val}|KeyValueList]}).


%%TODO either alter mochijson to handle abitrarily complex values from mnesia or handle it here

attribute_names(Table) when is_list(Table) ->
    attribute_names(list_to_atom(Table));

attribute_names(Table) when is_atom(Table) ->
    Attributes = mnesia:table_info(Table,  attributes),
    lists:map(fun(Attr) -> atom_to_list(Attr) end, Attributes).
		
record(Table, AttrList) when is_atom(Table) ->
    record(atom_to_list(Table), AttrList);
record(Table, AttrList) when is_list(AttrList) ->
    "-record(" ++ string:to_lower(Table)  ++", {" ++ string:join(AttrList, ", ") ++ " }).".

%%-----------------------------------------------------------------------------------------------
%%Function:    query_func/1
%%Description: returns the function code to execute the list comprehension query, without order
%%-----------------------------------------------------------------------------------------------
query_func(Comprehension) ->
     atom_to_list(?TEMPORARY_FUNCTION) 
	++ "() ->  mnesia:transaction(fun() ->qlc:e(qlc:q(" 
	++ Comprehension 
	++ ")) end).".

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

initcap([First|T]) ->
    [string:to_upper(First)|string:to_lower(T)].


format_operand(Table, Op) when is_number(Op) ->
    Op;
format_operand(Table, Op) when is_binary(Op) ->
    format_operand(Table, binary_to_list(Op));
format_operand(Table, Op) when is_list(Op) ->
    Attr = attribute_names(Table),
    case lists:member(string:to_lower(Op), Attr) of
	true -> column(Table, Op);
	false -> Op
    end.
    
