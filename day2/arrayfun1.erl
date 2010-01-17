-module(arrayfun1).

-compile(export_all).

test(N) ->
    5 * N.

is_message(<<"%%<:message:", _Rest/binary>>) ->
    true;
is_message(_Msg) ->
    false.

parse(String) ->
    string:tokens(String, ":").

