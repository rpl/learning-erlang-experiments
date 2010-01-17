%% yate_decode: yate message decoding experimental erlang module.
%%
%% Copyright (C) 2009 - Alca Società Cooperativa <info@alcacoop.it>
%%
%% Author: Luca Greco <luca.greco@alcacoop.it>
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% General Public License for more details.
%%
%% You should have received a copy of the GNU Lessel General Public License
%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

-module(yate_decode).

-compile(export_all).

-include("yate.hrl").

%%% NOTE: decode message type and call specialized attributes parsing functions
binary_to_term(Raw = <<"Error in: ", Msg/binary>>) when is_binary(Raw) ->
    #yate_event{ direction=incoming, type=error, attrs=[{msg, binary_to_list(Msg)}] };
binary_to_term(Raw = <<"%%<install:", Rest/binary>>) when is_binary(Raw) -> 
    #yate_event{ direction=answer, type=install, attrs=decode_install_answer_attributes(Rest) };
binary_to_term(Raw = <<"%%<uninstall:", Rest/binary>>) when is_binary(Raw) -> 
    #yate_event{ direction=answer, type=uninstall, attrs=decode_uninstall_answer_attributes(Rest) };
binary_to_term(Raw = <<"%%<watch:", Rest/binary>>) when is_binary(Raw) -> 
    #yate_event{ direction=answer, type=watch, attrs=decode_watch_answer_attributes(Rest) };
binary_to_term(Raw = <<"%%<unwatch:", Rest/binary>>) when is_binary(Raw) -> 
    #yate_event{ direction=answer, type=unwatch, attrs=decode_unwatch_answer_attributes(Rest) };
binary_to_term(Raw = <<"%%<setlocal:", Rest/binary>>) when is_binary(Raw) -> 
    #yate_event{ direction=answer, type=setlocal, attrs=decode_setlocal_answer_attributes(Rest) };
binary_to_term(Raw = <<"%%<message:", Rest/binary>>) when is_binary(Raw) -> 
    [ EventAttrs, MsgParams ] = decode_message_answer_attributes(Rest),
    #yate_event{ direction=answer, type=message, attrs=EventAttrs, params=MsgParams };
binary_to_term(Raw = <<"%%>message:", Rest/binary>>) when is_binary(Raw) ->
    [ EventAttrs, MsgParams ] = decode_message_incoming_attributes(Rest),
    #yate_event{ direction=incoming, type=message, attrs=EventAttrs, params=MsgParams };
binary_to_term(_Unknown) when is_binary(_Unknown) ->
    ?THROW_YATE_EXCEPTION(unknown_event, "Unkown YATE Event", _Unknown);
binary_to_term(_Unknown) ->
    ?THROW_YATE_EXCEPTION(invalid_data, "Needs binary data", _Unknown).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% NOTE: private specialized attributes parsing functions
%%%       on error throws 
%%%  { invalid_data, { data, Data }, { where, File, Line } }
%%%       unimplemented features throws
%%%  { not_implemented, {data, Rest}, { where, ?FILE, ?LINE } }
%%%
%%% IMPLEMENTATION NOTES:
% throw({ not_implemented, {data, Rest}, { where, ?FILE, ?LINE } }).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode_install_answer_attributes(Rest) when is_binary(Rest) ->
    case string:tokens(binary_to_list(Rest), ":") of
	[ Priority, Name, Success ] -> [ { priority, Priority }, { name, Name }, { success, Success } ];
        _Any -> ?THROW_YATE_EXCEPTION(invalid_data, "Error parsing install answer attributes", _Any)
%% throw({ invalid_data, {data, _Any}, { where, ?FILE, ?LINE } })
    end.

decode_uninstall_answer_attributes(Rest) when is_binary(Rest) ->
    case string:tokens(binary_to_list(Rest), ":") of
	[ Name, Success ] -> [ { name, Name }, { success, Success } ];
        _Any -> ?THROW_YATE_EXCEPTION(invalid_data, "Error parsing install answer attributes", _Any)
    end.

decode_watch_answer_attributes(Rest) when is_binary(Rest) ->
    case string:tokens(binary_to_list(Rest), ":") of
	[ Name, Success ] -> [ { name, Name }, { success, Success } ];
        _Any -> ?THROW_YATE_EXCEPTION(invalid_data, "Error parsing install answer attributes", _Any)
    end.

decode_unwatch_answer_attributes(Rest) when is_binary(Rest) ->
    case string:tokens(binary_to_list(Rest), ":") of
	[ Name, Success ] -> [ { name, Name }, { success, Success } ];
        _Any -> ?THROW_YATE_EXCEPTION(invalid_data, "Error parsing install answer attributes", _Any)
    end.

decode_setlocal_answer_attributes(Rest) when is_binary(Rest) ->
    case string:tokens(binary_to_list(Rest), ":") of
	[ Name, Value, Success ] -> [ { name, Name }, { value, Value }, { success, Success } ];
        _Any -> ?THROW_YATE_EXCEPTION(invalid_data, "Error parsing install answer attributes", _Any)
    end.

%%% %%<message:<id>:<processed>:[<name>]:<retvalue>[:<key>=<value>...]
decode_message_answer_attributes(Rest) when is_binary(Rest) ->
    case string:tokens(binary_to_list(Rest), ":") of
	[ Id, Processed, Name, RetVal | RawMsgParams ] -> 
	    Attrs = [ { id, Id }, { processed, Processed }, { name, Name }, { retval, RetVal }],
	    MsgParams = decode_message_parameters(RawMsgParams),
	    [Attrs, MsgParams];
        _Any -> ?THROW_YATE_EXCEPTION(invalid_data, "Error parsing install answer attributes", _Any)
    end.

%%% %%>message:<id>:<time>:<name>:<retvalue>[:<key>=<value>...]
decode_message_incoming_attributes(Rest) when is_binary(Rest) ->
    case string:tokens(binary_to_list(Rest), ":") of
	[ Id, Time, Name, RetVal | RawMsgParams ] -> 
	    Attrs = [ { id, Id }, { time, Time }, { name, Name }, { retval, RetVal }],
	    MsgParams = decode_message_parameters(RawMsgParams),
	    [Attrs, MsgParams];
        _Any -> ?THROW_YATE_EXCEPTION(invalid_data, "Error parsing install answer attributes", _Any)
    end.

decode_message_parameters([H|T] = RawMsgParams) when is_list(RawMsgParams) ->
    MsgParam = case string:tokens(H, "=") of
		   [Key, Value] -> { list_to_atom(Key), Value };
		   _Any -> ?THROW_YATE_EXCEPTION(invalid_data, "Error parsing install answer attributes", _Any)
	       end,
    [MsgParam | decode_message_parameters(T)];
decode_message_parameters([]) ->
    [].

