-module(grove_mnesia_ops).

-export([lt/2,
	 lte/2, 
	 gt/2, 
	 gte/2, 
	 eq/2, 
	 neq/2]).

-define(NOTEST, 1).
-define(OPERAND_ALLOW_REGEX, "^[0-9\.a-zA-Z#\"]+$").

-include_lib("eunit/include/eunit.hrl").
%%-----------------------------------------------------------------------------------------------
%%Function:    lt
%%Description: returns the string of a qualifier for a check of < for the left and right 
%%             arguments
%%-----------------------------------------------------------------------------------------------
lt(Left, Right) ->
    lt([Left, Right]).

lt([_l, _r] = Ops) ->
    format_operation("~s < ~s", Ops).

%%-----------------------------------------------------------------------------------------------
%%Function:    lte
%%Description: returns the string of a qualifier for a check of <= for the left and right 
%%             arguments
%%-----------------------------------------------------------------------------------------------
lte(Left, Right) ->
    lte([Left, Right]).

lte([_l, _r] = Ops) ->
    format_operation("~s =< ~s", Ops).

%%-----------------------------------------------------------------------------------------------
%%Function:    gt
%%Description: returns the string of a qualifier for a check of > for the left and right 
%%             arguments
%%-----------------------------------------------------------------------------------------------
gt(Left, Right) ->
    gt([Left, Right]).

gt([_l, _r] = Ops) ->
    format_operation("~s > ~s", Ops).

%%-----------------------------------------------------------------------------------------------
%%Function:    gte
%%Description: returns the string of a qualifier for a check of >= for the left and right 
%%             arguments
%%-----------------------------------------------------------------------------------------------
gte(Left, Right) ->
    gte([Left, Right]).

gte([_l, _r] = Ops) ->
    format_operation("~s >= ~s", Ops).

%%-----------------------------------------------------------------------------------------------
%%Function:    eq
%%Description: returns the string of a qualifier for a check of == for the left and right 
%%             arguments
%%-----------------------------------------------------------------------------------------------
eq(Left, Right) ->
    eq([Left, Right]).

eq([_l, _r] = Ops) ->
    format_operation("~s == ~s", Ops).

%%-----------------------------------------------------------------------------------------------
%%Function:    neq
%%Description: returns the string of a qualifier for a check of =/= for the left and right 
%%             arguments
%%-----------------------------------------------------------------------------------------------
neq(Left, Right) ->
    neq([Left, Right]).

neq([_l, _r] = Ops) ->
    format_operation("~s =/= ~s", Ops).

%%-----------------------------------------------------------------------------------------------
%%Function:    format_operation
%%Description: fits the operands into the operation format string
%%-----------------------------------------------------------------------------------------------
format_operation(Operation, Operands) ->
    OpStrs = grove_util:all_strings(Operands),
    ok = validate_operands(OpStrs),
    lists:flatten(io_lib:format(Operation, OpStrs)).

%%-----------------------------------------------------------------------------------------------
%%Function:    validate_operands
%%Description: validates each of the operands agains the defined regex. Mnesia will allow 
%%             possible less than mysql (ie " in (value1, value2) " ). Every element of the list
%%             must be strings
%%-----------------------------------------------------------------------------------------------
validate_operands([]) -> ok;

validate_operands([Operand|T]) ->
    case regexp:match(Operand, ?OPERAND_ALLOW_REGEX) of
	{match, _, _} ->  validate_operands(T);
	nomatch -> throw(invalid_operand_characters)
    end.

%-----Test-------

code_injection_test() ->    
    ?_assertException(throw, invalid_operand_characters, lt(";", 2)),
    ?_assertException(throw, invalid_operand_characters, lt("endofworld()", 2)).
    %need much more clever tests here

lt_test() ->
    "1 < 2" = lt(1, 2),
    "Shop < 2.0" = lt("Shop", 2.0).

lte_test() ->
    "1 =< 2" =  lte(1, 2),
    "Shop =< 20" =  lte("Shop", 20),
    "\"Shop\" =< 20" =  lte("\"Shop\"", 20).

gt_test() ->
    "1 > 2" =  gt(1, 2),
    "Shop > 2.6552" =  gt("Shop", 2.6552).

gte_test() ->
    "1 >= 2" =  gte(1, 2),
    "Shop >= 20" =  gte("Shop", 20).

eq_test() ->
    "1 == 2" =  eq(1, 2),
    "Shop == 20" =  eq("Shop", 20).

neq_test() ->
    "1 =/= 2" =  neq(1, 2),
    "Shop =/= 20" =  neq("Shop", 20).

format_operation_test() ->
    "nothing -- nothing" = format_operation("~s -- ~s", [nothing, nothing]), 
    ?_assertException(throw, invalid_operand_characters, format_operation("", ["mod:code().", nothing])).

validate_operands_test() ->
    ok = validate_operands(["2", "2.3", "Table#table.column", "\"string\""]),
    ?_assertException(throw, invalid_operand_characters, validate_operands([","])),
    ?_assertException(throw, invalid_operand_characters, validate_operands([";"])),
    ?_assertException(throw, invalid_operand_characters, validate_operands(["("])),
    ?_assertException(throw, invalid_operand_characters, validate_operands([")"])),
    ?_assertException(throw, invalid_operand_characters, validate_operands([":"])),
    ?_assertException(throw, invalid_operand_characters, validate_operands(["."])).
