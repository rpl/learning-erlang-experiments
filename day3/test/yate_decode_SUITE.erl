-module(yate_decode_SUITE).

-compile(export_all).

-include("yate.hrl").

all() ->
    [
     decode_yate_errorin_event
    ].

decode_yate_errorin_event(_Config) ->
    ExpectedValue = #yate_event{
      direction=incoming,
      type=error,
      attrs=[{msg,"Test YATE error event decoding"}]
     },
    ExpectedValue = yate_decode:binary_to_term(<<"Error in: Test YATE error event decoding">>).
