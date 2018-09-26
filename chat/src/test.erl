%% test file

-module(test).

-import(string, [concat/2, len/1]).

-export([main/0]).

main() ->
    Str1 = "Random stirng",
    Str2 = "Another string",
    io:fwrite("String: ~p ~p\n", [Str1, Str2]),
    Str3 = io_lib:format("It's a ~s and ~s\n",
			 [Str1, Str2]),
    len(Str3),
    Str4 = concat(Str1, Str2),
    io:fwrite(Str3),
    io:fwrite(Str4).

% internal functions

% var_stuff() -> Num = 1, Num.

% do_math(A, B) ->
%     A + B, A * B, A - B, math:log(9000), math:log(9).

% compare(A, B) -> A == B.

