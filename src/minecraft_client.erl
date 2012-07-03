%%%-----------------------------------------------------------------------------
%%% Use is subject to License terms.
%%% @copyright (C) 2012 Our Robot Shop, Inc.
%%% @doc Module to repsent a Minecraft Client.
%%% @end
%%%-----------------------------------------------------------------------------

-module(minecraft_client).
-author("Our Robot Shop, Inc.").

-include("../include/minecraft_client.hrl").

-behavior(gen_server).

-compile([export_all]).

%% API
-export([start_link/1,
	 connect/0,
	 connect/2,
         connect/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%-----------------------------------------------------------------------------
%%% API functions
%%%-----------------------------------------------------------------------------

%% @doc Start Minecraft Client.
-spec start_link( Args :: term()) -> ok.
start_link(_Args) ->
    gen_server:start_link(?MODULE, _Args, []),
    ok.

start() ->
    spawn(?MODULE, connect, []).

%% @doc Connect to a Minecraft server on the localhost with at default port of 25565
-spec connect() -> ok.
connect() -> connect(?DEFAULT_MC_SERVER,?DEFAULT_MC_PORT,?DEFAULT_MC_CLIENT_USERNAME).

connect(Server,Username)->
    connect(Server,?DEFAULT_MC_PORT,Username).

-spec connect(string(), integer(), string()) -> ok.
connect(Server, Port, Username)->
    {ok, Sock} = gen_tcp:connect(Server, Port,
				 [binary,{packet,raw},{keepalive,true}]),
    HandshakePacket = mcp:write_packet({handshake,
				       [{username_and_host, ?HANDSHAKE_UAH_STR(Username,Server,Port)}]}),
    ok = gen_tcp:send(Sock, HandshakePacket),
    receive
	{tcp, _RSocket, RPacket} -> ok
    end,
    HandshakeResponse = mcp:read_packet(RPacket),
    io:format("From Server:~n~p~n",[HandshakeResponse]),
    LoginPacket = mcp:write_packet({login_request,
				       [{proto_version,?DEFAULT_MC_PROTO_VERSION},
					{username,Username}]}),					     
    ok = gen_tcp:send(Sock, LoginPacket),
    receive
	{tcp, _RSocket2, RPacket2} -> ok
    end,
    LoginResponse = mcp:read_packet(RPacket2),
    io:format("From Server:~n~p~n",[LoginResponse]),
%    mcp:write_packet({keep_alive,
%		      [{keep_alive_id, 0}]}),
%    receive
%	{tcp, _RSocket3, RPacket3} -> ok
%    end,
%    Response3 = mcp:read_packet(RPacket3),
%    io:format("From Server:~n~p~n",[Response3]).

    timer(),    
    run(Sock).

run(Sock) ->
    receive
	{tcp, _RSocket, RPacket} ->
	    try
		Response = mcp:read_packet(RPacket)
		%io:format("From Server:~n~p~n",[Response])
	    catch
		Error -> io:format("Error: ~p~n",Error)
	    end,
	    run(Sock);
	{to_server,Message} ->
	    MessagePacket = mcp:write_packet(Message),
	    ok = gen_tcp:send(Sock,MessagePacket),
	    run(Sock);
	timer_expire -> KeepAlivePacket = mcp:write_packet({keep_alive,
					  [{keep_alive_id, 0}]}),
			ok = gen_tcp:send(Sock, KeepAlivePacket),
			run(Sock)
    end.

timer() ->
    spawn(?MODULE, timer, [self()]).
timer(Pid) ->
    receive
	reset -> timer(Pid)
    after
	5000 ->
	    Pid ! timer_expire,
	    timer(Pid)
    end.
    
move(Pid, X, Y, Z, Grounded) ->
    Pid ! {to_server,{player_position,[{x,X},{y,Y},{stance,Y},{z,Z},{on_ground,Grounded}]}}.

chat(Pid, Message) ->
    Pid ! {to_server,{chat_message,[{message,Message}]}}.
    
%%%-----------------------------------------------------------------------------
%%% gen_server callbacks
%%%-----------------------------------------------------------------------------

%% @private
-spec init(list(tuple())) -> ignore.
init(_Args) -> ignore.

%% @private
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, Args :: term()) -> ok.
handle_call(default_connect, _From, _Args) ->
    connect().

%% @private
-spec handle_cast(Msg :: term(), Args :: term()) -> ignore.
handle_cast(_Msg, _Args) ->
    %% FIXME: implement
    ignore.

%% @private
-spec handle_info(Info :: term(), Args :: term()) -> ignore.
handle_info(_Info, _Args) ->
    %% FIXME: implement
    ignore.


%% @private
-spec terminate(Reason :: term(), Args :: term()) -> ok.
terminate(_Reason, _Args) -> 
    %% FIXME: implement
    ok.
 

%% @private
-spec code_change(Vsn :: term(), Args :: term() , Extra :: term()) -> ok.
code_change(_OldVsn, _Args, _Extra) -> 
    %% FIXME: implement
    ok.


%%%-----------------------------------------------------------------------------
%%% Utilities
%%%-----------------------------------------------------------------------------



