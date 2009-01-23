
-module(test_grove).

-include_lib("eunit/include/eunit.hrl").

lt_test() ->
    "1 < 2" = grove_mnesia:lt(1, 2),
    "Shop < 20" = grove_mnesia:lt("Shop", 20),
    ?_assertException(throw, invalid_operand_characters, grove_mnesia:lt(";", 2)),
    ?_assertException(throw, invalid_operand_characters, grove_mnesia:lt("apoc:endofworld().", 2)).

lte_test() ->
    "1 <= 2" = grove_mnesia:lte(1, 2),
    "Shop <= 20" = grove_mnesia:lte("Shop", 20).

gt_test() ->
    "1 > 2" = grove_mnesia:gt(1, 2),
    "Shop > 20" = grove_mnesia:gt("Shop", 20).

gte_test() ->
    "1 >= 2" = grove_mnesia:gte(1, 2),
    "Shop >= 20" = grove_mnesia:gte("Shop", 20).

eq_test() ->
    "1 > 2" = grove_mnesia:eq(1, 2),
    "Shop > 20" = grove_mnesia:eq("Shop", 20).
