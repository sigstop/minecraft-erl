%%%-----------------------------------------------------------------------------
%%% Use is subject to License terms.
%%% @copyright (C) 2012 Our Robot Shop, Inc.
%%% @doc Module to repsent a Minecraft Client.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mcc_message).
-author("Our Robot Shop, Inc.").

-include("../include/minecraft_client.hrl").

-compile([export_all]).

player_position_and_look(Pid,Client,Values) ->
    [{x,X},
     {stance,Stance},
     {y,Y},
     {z,Z},
     {yaw,Yaw},
     {pitch,Pitch},
     {on_ground,Grounded}] = Values,
    {player_position_and_look,
     [{x,CurX},
     {y,CurY},
     {stance,_CurStance},
     {z,CurZ},
     {yaw,_CurYaw},
     {pitch,_CurPitch},
     {on_ground,_CurGrounded}]} = Client#mcc_state.pos_look,
    DeltaStance = Stance-Y,
    DeltaX = abs(X-CurX),
    DeltaY = abs(Y-CurY),
    DeltaZ = abs(Z-CurZ),
    Legal = (DeltaX < 100.0) and (DeltaY < 100.0) and (DeltaZ < 100.0) and (DeltaStance < 1.65) and (DeltaStance > 0.1),
    Initial = (CurX == 0.0) and (CurY == 0.0) and (CurZ == 0.0),
    case Legal or Initial of
	true ->
	    minecraft_client:move_and_look(Pid,X,Y,Stance,Z,Yaw,Pitch,Grounded);
	_ -> io:format("Decoding error: illegal move ~p~n",[Values])
    end.
	    
	    

player_position(Pid,Client,Values) ->
    [{x,X},
     {stance,Stance},
     {y,Y},
     {z,Z},
     {on_ground,Grounded}] = Values,
    {player_position_and_look,
     [{x,CurX},
      {y,CurY},
      {stance,_CurStance},
      {z,CurZ},
      {yaw,_CurYaw},
      {pitch,_CurPitch},
      {on_ground,_CurGrounded}]} = Client#mcc_state.pos_look,
    DeltaStance = Stance-Y,
    DeltaX = abs(X-CurX),
    DeltaY = abs(Y-CurY),
    DeltaZ = abs(Z-CurZ),
    Legal = (DeltaX < 100.0) and (DeltaY < 100.0) and (DeltaZ < 100.0) and (DeltaStance < 1.65) and (DeltaStance > 0.1),
    Initial = (CurX == 0.0) and (CurY == 0.0) and (CurZ == 0.0),
    case Legal or Initial of
	true ->
	    minecraft_client:move(Pid,X,Y,Stance,Z,Grounded);
	_ -> io:format("Decoding error: illegal move ~p~n",[Values])
    end.




