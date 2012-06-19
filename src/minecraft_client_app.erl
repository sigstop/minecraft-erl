%%%-----------------------------------------------------------------------------
%%% Use is subject to License terms.
%%% @copyright (C) 2012 Our Robot Shop, Inc.
%%% @doc Callback module for Minecraft Client application.
%%% @end
%%%-----------------------------------------------------------------------------
-module(minecraft_client_app).
-author("Our Robot Shop, Inc.").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

%% @doc Start the application.
start(_StartType, _StartArgs) ->
    minecraft_client_sup:start_link().

%% @doc Stop the application
stop(_State) ->
    ok.
