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

-include("yate.hrl").

%%% NOTE: decode message type and call specialized attributes parsing functions
term_to_binary(YateEvt) when is_record(YateEvt,yate_event) ->
    YateEvt;
term_to_binary(_Any) ->
    "ERROR".

