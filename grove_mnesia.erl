-module(grove_mnesia).
-compile(export_all).
%-behaviour(grove_behaviour).

-define(OPERATION, "~s ~s ~s").
%%may not restrict ( or ) for grove_mysql because of in statement, we will see
-define(OPERAND_RESTRICT_REGEX, "[\|,;\(\)]|(\.\s)").

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
    lists:flatten(io_lib:format(Operation, Operands)).

%%-----------------------------------------------------------------------------------------------
%%Function:    validate_operands
%%Description: validates each of the operands agains the defined regex. Mnesia will allow 
%%             possible less than mysql (ie " in (value1, value2) " )
%%-----------------------------------------------------------------------------------------------
validate_operands([]) -> ok;
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
    Expresion = format_query({parts, 
		  {table, Object}, 
		  {columns, all}, 
		  {operations, [eq(column(Object, item), ID)]}, 
		  {order, []}}),
    FunString = "run_fun() ->  mnesia:transaction(fun() ->" ++ Expresion ++ " end).",
%    io:format(FunString),
%    ModAtom = list_to_atom("test_add_fun"),
    Mod = smerl:new(add_fun),

    Attributes = mnesia:table_info(list_to_atom(Object),  attributes),
    AttrList = lists:map(fun(Attr) -> atom_to_list(Attr) end, Attributes) ,
    {ok, RecAdded} = smerl:add_rec(Mod, "-record(" 
                                        ++ 
                                        string:to_lower(Object) 
                                        ++ 
                                        ", {" 
                                        ++
                                        string:join(AttrList, ", ")
                                        ++
                                        " })."),
    {ok, InclAdded} = smerl:add_incl(RecAdded, "/opt/erlang/lib/erlang/lib/stdlib-1.15.5/include/qlc.hrl", qlc),
    {ok, FunAdded} = smerl:add_func(InclAdded, FunString),
%    io:fwrite("~w~n", [FunAdded]),
    ok = smerl:compile(FunAdded),
    {atomic, Result} = add_fun:run_fun(),
    format_json(Result, AttrList).
%%-----------------------------------------------------------------------------------------------
%%Function:    format_query/2
%%Description: deals with the query Parts tuple by placing the resulting values for each element
%%             into the same position in the tuple and passing it on. Finally it returns the 
%%             set comprehension string that constitutes the query from the Parts specified
%%-----------------------------------------------------------------------------------------------
format_query({parts, {table, Name}, Col, Op, Ord}) ->
    ok = validate_operands([Name]),
    Tab = "X <- mnesia:table(" ++ string:to_lower(Name) ++ ")",
    format_query({parts, Tab, Col, Op, Ord});

format_query({parts, Tab, {columns, all}, Op, Ord}) ->
    format_query({parts, Tab, "X || ", Op, Ord});

format_query({parts, Tab, {columns, Object, Columns}, Op, Ord}) ->
    format_query({parts, Tab, format_columns(Object, Columns), Op, Ord});

format_query({parts, Tab, Col, {operations, []}, Ord}) ->
    format_query({parts, Tab, Col, " ", Ord});

format_query({parts, Tab, Col, {operations, Ops}, Ord}) ->
    format_query({parts, Tab, Col, "," ++ string:join(Ops, ", "), Ord});

format_query({parts, Tab, Col, Op, {order, Ord}}) ->
    format_query({parts, Tab, Col, Op, ""});

%%!!must be last to finalize the formatted query, the ordering might take place
format_query({parts, Tab, Col, Op, Ord}) -> "qlc:e(qlc:q([" ++ string:join([Col, Tab, Op], " ") ++ "]))".
    


%%-----------------------------------------------------------------------------------------------
%%Function:    format_columns/2
%%Description: Returns the columns list string necessary for limiting those columns in an mnesia
%%             query string
%%-----------------------------------------------------------------------------------------------
format_columns(Object, Columns) ->
    format_columns(Object, Columns, "").

%%-----------------------------------------------------------------------------------------------
%%Function:    format_columns/3
%%Description: creates the correct string for selecting specific columns in a set comprehension 
%%-----------------------------------------------------------------------------------------------
format_columns(_object, [], ColumnList) ->
    "{" ++ string:join(ColumnList, ", ") ++ "} ||";
format_columns(Object, [Column|T], ColumnList) ->
    format_columns(Object, 
		   T, 
		   [column(Object, Column)|ColumnList]).



%%-----------------------------------------------------------------------------------------------
%%Function:    column/2
%%Description: returns the string representing a column in an mnesia query, built from the 
%%             Record and Field name
%%-----------------------------------------------------------------------------------------------
column(Record, Field) when is_atom(Field) -> 
    "X#" ++ Record ++ "." ++ atom_to_list(Field);
column(Record, Field) ->
    "X#" ++ Record ++ "." ++ Field.
    
format_json(Rows, Attributes) when is_list(Rows), is_list(Attributes) ->
    format_json(Rows, Attributes, []).

format_json([], _, JSONArray) ->
    io:fwrite("~w~n", [JSONArray]),
    mochijson2:encode(JSONArray);

format_json([Row|T], Attributes, JSONArray) when is_tuple(Row)->
    format_json(T, Attributes, [format_struct(Attributes, tuple_to_list(Row))|JSONArray]).



format_struct(Attributes, [_table|Values])when is_list(Values), is_list(Attributes) ->
    format_struct(Attributes, Values, {struct, []}).

format_struct([], [], Struct) -> Struct;

format_struct([Attr|T], [Val|T2], {struct, KeyValueList}) ->
    format_struct(T, T2, {struct, [{Attr, Val}|KeyValueList]}).
    
					   
