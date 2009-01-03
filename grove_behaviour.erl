-module(grove_behaviour).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{eq, 2},
     {format_operation, 2},
     {gt, 2},
     {gte, 2},
     {lt,2},
     {lte, 2},
     {neq, 2},
     {validate_operands, 1}];

behaviour_info(_Other) ->
    undefined.
