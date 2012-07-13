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
start(Server,Username) ->
    spawn(?MODULE, connect, [Server, Username]).

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
    {[Login|_Results],_Error,_Remainder} = LoginResponse,
    {login_request,[{eid,MyEID},
                  {empty,_},
                  {level_type,_},
                  {server_mode,_},
                  {dimension,_},
                  {difficulty,_},
                  {empty,_},
                  {max_players,_}]} = Login,
    io:format("From Server:~n~p~n",[LoginResponse]),
    io:format("MyEID:~n~p~n",[MyEID]),
    EIDWatchList = [MyEID],
%    mcp:write_packet({keep_alive,
%		      [{keep_alive_id, 0}]}),
%    receive
%	{tcp, _RSocket3, RPacket3} -> ok
%    end,
%    Response3 = mcp:read_packet(RPacket3),
%    io:format("From Server:~n~p~n",[Response3]).

    %timer(),   
    LP = {player_position_and_look,[{x,0.0},
				    {y,0.0},
				    {stance,0.9},
				    {z,0.0},
				    {yaw,1.0},
				    {pitch,1.0},
				    {on_ground,true}]}, 
    LPPacket = mcp:write_packet(LP),
    Client = #mcc_state{eid=MyEID,eid_watch_list=EIDWatchList,socket=Sock,pos_look=LP,lp_packet=LPPacket,initalized=false},
    run(Client).

run(Client) ->
    receive
	{tcp, _RSocket, RPacket} ->
	    try
		Response = mcp:read_packet(RPacket),
	        %io:format("From Server:~n~p~n",[Response]),
		case Response of 
		    {Results,_Error,_Remaider} -> ok;
		    _ -> Results = Response
		end,
		handle_results(Client,Results),
		run(Client)
	    catch
		Error -> io:format("Error: ~p~n",Error)
	    end,
	    run(Client);
	{to_server,{player_position_and_look,[{x,X},
					      {y,Y},
					      {stance,Stance},
					      {z,Z},
					      {yaw,Yaw},
					      {pitch,Pitch},
					      {on_ground,Grounded}]}} -> 
	    NewLP = {player_position_and_look,[{x,X},
					       {y,Y},
					       {stance,Stance},
					       {z,Z},
					       {yaw,Yaw},
					       {pitch,Pitch},
					       {on_ground,Grounded}]},
	    NewLPPacket = mcp:write_packet(NewLP),
	    io:format("To Server: ~p~n",[NewLPPacket]),
	    ok = gen_tcp:send(Client#mcc_state.socket,NewLPPacket),
	    UpdatedClient = Client#mcc_state{pos_look=NewLP,lp_packet=NewLPPacket},
	    run(UpdatedClient);
	{to_server,Message} ->
	    MessagePacket = mcp:write_packet(Message),
	    io:format("To Server: ~p~n",[MessagePacket]),
	    ok = gen_tcp:send(Client#mcc_state.socket,MessagePacket),
	    run(Client);
	{move_x,DeltaX} ->
	    {player_position_and_look,
	     [{x,X},
	      {y,Y},
	      {stance,Stance},
	      {z,Z},
	      {yaw,Yaw},
	      {pitch,Pitch},
	      {on_ground,Grounded}]} = Client#mcc_state.pos_look,
	    NewLP = {player_position_and_look,[{x,X+DeltaX},
					       {y,Y},
					       {stance,Stance},
					       {z,Z},
					       {yaw,Yaw},
					       {pitch,Pitch},
					       {on_ground,Grounded}]},
	    NewLPPacket = mcp:write_packet(NewLP),
	    io:format("To Server: ~p~n",[NewLPPacket]),
	    ok = gen_tcp:send(Client#mcc_state.socket,NewLPPacket),
	    UpdatedClient = Client#mcc_state{pos_look=NewLP,lp_packet=NewLPPacket},
	    run(UpdatedClient);
	{move_z,DeltaZ} ->
	    {player_position_and_look,
	     [{x,X},
	      {y,Y},
	      {stance,Stance},
	      {z,Z},
	      {yaw,Yaw},
	      {pitch,Pitch},
	      {on_ground,Grounded}]} = Client#mcc_state.pos_look,
	    NewLP = {player_position_and_look,[{x,X},
					       {y,Y},
					       {stance,Stance},
					       {z,Z+DeltaZ},
					       {yaw,Yaw},
					       {pitch,Pitch},
					       {on_ground,Grounded}]},
	    NewLPPacket = mcp:write_packet(NewLP),
	    io:format("To Server: ~p~n",[NewLPPacket]),
	    ok = gen_tcp:send(Client#mcc_state.socket,NewLPPacket),
	    UpdatedClient = Client#mcc_state{pos_look=NewLP,lp_packet=NewLPPacket},
	    run(UpdatedClient);
	{watch_eid,WatchEID} ->
	    NewEIDWatchList = lists:append(Client#mcc_state.eid_watch_list,[WatchEID]),
	    UpdatedClient = Client#mcc_state{eid_watch_list=NewEIDWatchList},
	    run(UpdatedClient);
	timer_expire -> KeepAlivePacket = mcp:write_packet({keep_alive,
					  [{keep_alive_id, 0}]}),
			ok = gen_tcp:send(Client#mcc_state.socket, KeepAlivePacket),
			run(Client)
    after
	50 -> 
	    %io:format("To Server (LP): ~p~n",[Client#mcc_state.lp_packet]),
	    ok = gen_tcp:send(Client#mcc_state.socket,Client#mcc_state.lp_packet),
	    run(Client)
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
    
chat(Pid, Message) ->
    Pid ! {to_server,{chat_message,[{message,Message}]}}.

look(Pid, Yaw, Pitch, Grounded) ->
    Pid ! {to_server,{player_look,[{yaw,Yaw},{pitch,Pitch},{on_ground,Grounded}]}}.

move(Pid, X, Y, Z, Grounded) ->
    Pid ! {to_server,{player_position,[{x,X},{y,Y},{stance,Y+1},{z,Z},{on_ground,Grounded}]}}.

move_x(Pid, DeltaX) ->
    Pid ! {move_x,DeltaX}.

move_z(Pid, DeltaZ) ->
    Pid ! {move_z,DeltaZ}.

move(Pid, X, Y, Stance, Z, Grounded) ->
    Pid ! {to_server,{player_position,[{x,X},{y,Y},{stance,Stance},{z,Z},{on_ground,Grounded}]}}.

move_and_look(Pid, X, Y, Stance, Z, Yaw, Pitch, Grounded) -> 
    Pid ! {to_server,{player_position_and_look,[{x,X},
						{y,Y},
						{stance,Stance},
						{z,Z},
						{yaw,Yaw},
						{pitch,Pitch},
						{on_ground,Grounded}]}}.

quit(Pid) ->
    Pid ! {to_server,{disconnect,[{reason,"done"}]}}.

watch(Pid,EID)->
    Pid ! {watch_eid,EID}.

% Iterate through the results 
% !NACK! should use BIF list handling like forall
handle_results(Client,Results) ->
    case Results of
	[] ->
	    ok;
	[Message|RemainingResults] -> handle_message(Client,Message),
				      handle_results(Client,RemainingResults)
    end.

handle_message(Client,Message) ->
    case Message of
	{player_position_and_look,Values} ->
	    io:format("From Server: ~p~n",[Message]),
	    spawn(mcc_message, player_position_and_look, [self(),Client,Values]);
	{player_position,Values} ->
	    io:format("From Server: ~p~n",[Message]),
	    spawn(mcc_message, player_position, [self(),Client,Values]);
	{chat_message,Values} ->
		io:format("From Server: ~p~n",[Values]);
	{disconnect,Values} ->
		io:format("Disconnecting: ~p~n",[Values]);
	{entity_look_and_relative_move,_Values} -> ok;
	%io:format("From Server: ~p~n",[Message]);
	{_Type,[{eid,EID}|_Rest]} ->
	    case lists:member(EID,Client#mcc_state.eid_watch_list) of
		true ->
		    io:format("About ~p: ~p~n",[EID,Message]);
		_ -> ok
	    end;
	_ -> ok
    end.

	    
	    
    
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



