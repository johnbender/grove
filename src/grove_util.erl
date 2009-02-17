%% The MIT License

%% Copyright (c) <year> <copyright holders>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(grove_util).
-compile(export_all).


initcap([First|T]) ->
    [string:to_upper(First)|string:to_lower(T)];

initcap(NonString) ->
    initcap(to_string(NonString)).

to_string(Object) when is_number(Object) ->
    lists:flatten(io_lib:format("~w" , [Object]));

to_string(Object) when is_atom(Object) ->
    atom_to_list(Object); 

to_string(Object) when is_binary(Object) ->
    binary_to_list(Object);

to_string(Object) when is_list(Object) ->
    Object.

to_lower_string(Object) ->
    string:to_lower(to_string(Object)).

%% As pointed out by StoneCypher in #erlang this is not the fastest implementation
%% the lists its working on are lists of table columns so they should be fairly short
%% for a faster implementaion see scutil (sorted/compared). 
intersection(A, B)when is_list(A), is_list(B) ->
    [X || X <- A, Y <- B, X == Y].


all_strings(Objects) ->
    lists:map(fun(X)-> to_string(X) end, Objects). 


all_lower_strings(Objects) ->
    lists:map(fun(X)-> to_lower_string(X) end, Objects).


sfrmt(Format, Strings) when is_list(Strings) ->
    lists:flatten(io_lib:format(Format, all_strings(Strings))).
