%%----------------------------------------------------------------
%% Copyright (c) 2023 vzroman@gmail.com
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%----------------------------------------------------------------
-module(eroaring).

-define(EMPTY,<<0,0,0,0,0,0,0,0>>).
%%==============================================================================
%%	API
%%==============================================================================
-export([
   create/0,
   from_list/1,
   to_list/1,

   intersection/2,
   union/2,
   subtract/2,

   add_elements/2,
   remove_elements/2,
   contains/2,
   count/1,
   is_empty/1
]).

%%==============================================================================
%%	API
%%==============================================================================
-compile(no_native).
-on_load(on_load/0).

-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).


create()->
    ?nif_stub.

from_list( _List )->
    ?nif_stub.

to_list( _Set )->
    ?nif_stub.

intersection( _Set1, _Set2 )->
    ?nif_stub.

union( _Set1, _Set2 )->
    ?nif_stub.

subtract( _Set1, _Set2 )->
    ?nif_stub.

add_elements( _Set, _Bits )->
    ?nif_stub.

remove_elements( _Set, _Bits )->
    ?nif_stub.

contains( _Set, _Bit )->
    ?nif_stub.

count( _Set )->
    ?nif_stub.

is_empty( Set )->
    Set =:= ?EMPTY.


on_load() ->
  SoName = 
    case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                filename:join([filename:dirname(Filename),"../priv", "liberoaring"]);
                _ ->
                filename:join("../priv", "liberoaring")
            end;
        Dir ->
            filename:join(Dir, "liberoaring")
    end,
  erlang:load_nif(SoName, application:get_all_env(eroaring)).