%% yate_encode: yate message encoding experimental erlang module.
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

-module(yate_encode).

-compile(export_all).

%% import yate_event record definition, and other utils (yate exceptions helpers)  
-include("yate.hrl").

%% encode a yate message from a yate_event record, 
%% or throw datainvalid yate_exception
term_to_binary(YateEvt) when is_record(YateEvt,yate_event) ->
    encode_yate_event(YateEvt);
term_to_binary(_Any) ->
    ?THROW_YATE_EXCEPTION(datainvalid, "Argument should be a yate_event record", _Any).

%% private function to convert a yate_event in a binary string by type and direction 
%% NOTE: only existent "Application to Engine" yate events have been implemented
encode_yate_event(#yate_event{type=message, direction=outgoing, attrs=Attrs, params=MsgParams}) ->
    << "%%>", "message", (encode_attributes(message_outgoing, Attrs))/binary, 
     (encode_attributes(message_params, MsgParams))/binary>>;
encode_yate_event(#yate_event{type=message, direction=answer, attrs=Attrs, params=MsgParams}) ->
    << "%%<", "message", (encode_attributes(message_answer, Attrs))/binary, 
     (encode_attributes(message_params, MsgParams))/binary>>;
encode_yate_event(#yate_event{type=Type, direction=outgoing, attrs=Attrs}) ->
    StringType = atom_to_list(Type),
    << "%%>", (list_to_binary(StringType))/binary, (encode_attributes(Type, Attrs))/binary>>;
encode_yate_event(YateEvt) ->
    ?THROW_YATE_EXCEPTION(invalid_application_yate_event, "Invalid Application YATE Event", YateEvt).

%% fetch and encode attributes by type 
%%
%%% TODO: use dict:fetch and throw a YATE exception if attrs are invalid (not complete) 
%%%       or set a default value
encode_attributes(install, Attrs) ->
    AttrsDict = dict:from_list(Attrs),
    Name = dict:fetch(name, AttrsDict),
    Filters = case (dict:find(filters, AttrsDict)) of
		  {ok, RawFilters} -> lists:flatmap(fun({X,Y}) -> [X, Y]  end, RawFilters);
		  error -> [ ]
	      end,
    case (dict:find(priority, AttrsDict)) of
	{ok, Priority} -> join_event_chunks([Priority, Name] ++ Filters);
	error -> join_event_chunks(["", Name] ++ Filters)
    end;
encode_attributes(uninstall, Attrs) ->
    encode_attributes(name_attr, Attrs);
encode_attributes(watch, Attrs) ->
    encode_attributes(name_attr, Attrs);
encode_attributes(unwatch, Attrs) ->
    encode_attributes(name_attr, Attrs);
encode_attributes(setlocal, Attrs) ->
    AttrsDict = dict:from_list(Attrs),
    Name = dict:fetch(name, AttrsDict),
    Value = dict:fetch(value, AttrsDict),
    join_event_chunks([Name, Value]);
encode_attributes(output, Attrs) ->
    AttrsDict = dict:from_list(Attrs),
    Value = dict:fetch(value, AttrsDict),
    join_event_chunks([Value]);
encode_attributes(message_outgoing, Attrs) ->
    AttrsDict = dict:from_list(Attrs),
    Id = dict:fetch(id, AttrsDict),
    Time = dict:fetch(time, AttrsDict),
    Name = dict:fetch(name, AttrsDict),
    RetValue = case (dict:find(retvalue, AttrsDict)) of
		   {ok, Value} -> Value;
		   error -> [":"]
	       end,
    join_event_chunks([Id, Time, Name, RetValue]);
encode_attributes(message_answer, Attrs) ->
    AttrsDict = dict:from_list(Attrs),
    Id = dict:fetch(id, AttrsDict),
    Processed = dict:fetch(processed, AttrsDict),
    Name = case (dict:find(name, AttrsDict)) of
	       {ok, NameValue} -> NameValue;
	       error -> []
	   end,
    RetValue = case (dict:find(retvalue, AttrsDict)) of
		   {ok, Value} -> Value;
		   error -> [":"]
	       end,
    join_event_chunks([Id, Processed, Name, RetValue]);
encode_attributes(message_params, MsgParams) ->
    % map and join message params with a '=' character
    FlatMsgParams = lists:flatmap(fun({X,Y}) -> 
					  [(encode_chunk(X)),<<"=">>,(encode_chunk(Y))]  
				  end, MsgParams),
    << <<B/binary>> || B <- FlatMsgParams >>;
encode_attributes(name_attr, Attrs) ->
    % executed by all yate events with only the name attributes (uninstall, watch and unwatch)
    AttrsDict = dict:from_list(Attrs),
    Name = dict:fetch(name, AttrsDict),
    join_event_chunks([Name]).

%% join yate event chunks (with a ':' character) in a binary strean 
join_event_chunks([H]) ->
    << ":", (encode_chunk(H))/binary >>;
join_event_chunks([]) ->
    << >>;
join_event_chunks([H|T]) ->
    << (join_event_chunks([H]))/binary, (join_event_chunks(T))/binary >>.

%% conver to binary single chunks
%%% TODO: encode special chars (es. ':')
encode_chunk(C) when is_integer(C) ->
    list_to_binary(integer_to_list(C));
encode_chunk(C) when is_list(C) ->
    list_to_binary(C);
encode_chunk(C) when is_atom(C)->
    encode_chunk(atom_to_list(C));
encode_chunk(C) when is_binary(C) ->
    C.
