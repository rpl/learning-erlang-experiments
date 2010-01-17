-module(testio).

-compile(export_all).

testio() ->
    io:setopts(standard_io,[{binary,true}]),
    Line = io:get_line(standard_io, ""),
    io:fwrite(standard_io, "Output: ~s~n", [Line]). 

main(_) ->
    testio().
