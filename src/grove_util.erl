-module(grove_util).

-compile(export_all).

initcap([First|T]) ->
    [string:to_upper(First)|string:to_lower(T)].

to_string(Object) when is_number(Object) ->
    lists:flatten(io_lib:format("~w" , [Object]));
to_string(Object) when is_atom(Object) ->
    atom_to_list(Object); 
to_string(Object) when is_binary(Object) ->
    binary_to_list(Object);
to_string(Object) when is_list(Object) ->
    Object.

%% As pointed out by StoneCypher in #erlang this is not the fastest implementation
%% the lists its working on are lists of table columns so they should be fairly short
%% for a faster implementaion see scutil (sorted/compared). 
intersection(A, B)when is_list(A), is_list(B) ->
    [X || X <- A, Y <- B, X == Y].

all_strings(Objects) ->
    lists:map(fun(X)-> to_string(X) end, Objects). 

all_lower_strings(Objects) ->
    lists:map(fun(X)-> string:to_lower(to_string(X)) end, Objects).

sfrmt(Format, Strings) when is_list(Strings) ->
    lists:flatten(io_lib:format(Format, all_strings(Strings))).
