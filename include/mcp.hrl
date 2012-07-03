%%%-----------------------------------------------------------------------------
%%% Use is subject to License terms.
%%% @copyright (C) 2012 Our Robot Shop, Inc.
%%% @doc Header file for the Minecraft Protocol.
%%% @end
%%%-----------------------------------------------------------------------------



%% Helper Macros

%% This macro builds the magic string which populates 
%% the username_and_host field of the handshake packet
-define(HANDSHAKE_UAH_STR( Username, Server, Port ), Username ++ ";" ++ Server ++ ":" ++ integer_to_list(Port)).

-define(MC_PROTO_PACKET_TYPE_POS,1).
-define(MC_PROTO_ID_OFFSET,1).
-define(MC_PROTO_VALUE_POS_OFFSET,1).
-define(MC_PROTO, [
{ %% Keep Alive 0x00
 keep_alive,
 [keep_alive_id],   %% To Server (Encode)
 [keep_alive_id],   %% From Server (Decode)
 [int],
 0}, 

{%% Login Request 0x01
 login_request,
 [proto_version, username, empty,      empty,       empty,     empty,      empty, empty],  %% To Server (Encode)
 [entity_id,     empty,    level_type, server_mode, dimension, difficulty, empty, max_players],  %% From Server (Decode)
 [int,           string,   string,     int,         int,       byte,       ubyte, ubyte],
 1},

{%% Handshake 0x02
 handshake,
 [username_and_host], %% To Server (Encode)
 [connection_hash],  %% From Server (Decode)
 [string],
 16#2},

{%% Chat Message 0x03
 chat_message,
 [message], %% To Sever
 [message], %% From Server
 [string],
 16#3},

{%% Time Update 0x04
 time_update,
 [], %% One way
 [time],  %% From Server (Decode)
 [long],
 4},

{%% Entity Equipment 0x05
 entity_equipment,
 [], %% One way
 [entity_id, slot,  item_id, damage],  %% From Server (Decode)
 [int,       short, short,   short],
 5},

{%% Spawn Position 0x06
 spawn_position,
 [], %% One way
 [x,   y,   z],  %% From Server (Decode)
 [int, int, int],
 16#06},

{%% Player Position 0x0B
 player_position,
 [x,      y,      stance, z,      on_ground],  %% To Server
 [x,      y,      stance, z,      on_ground],  %% From Server
 [double, double, double, double, bool],
 16#0b},  

{%% Spawn Mob 0x18
 spawn_mob,
 [], %% One way
 [entity_id, type, x,   y,   z,   yaw,  pitch, head_yaw, metadata], %% From Server
 [int,       byte, int, int, int, byte, byte,  byte,     byte],
 16#18},

{%% Entity Velocity 0x1C
 entity_velocity,
 [], %% One way
 [entity_id, x,     y,     z], %% From Server
 [int,       short, short, short],
 16#1c},

{%% Entity Relative Move 0x1F
 entity_relative_move,
 [], %% One way
 [eid, dx,   dy,   dz],  %% From Server
 [int, byte, byte, byte],
 16#1F},

{%% Entity Look 0x20
 entity_look,
 [], %% One way
 [eid, yaw,  pitch],  %% From Server
 [int, byte, byte],
 16#20},

{%% Entity Look and Relative Move 0x21
 entity_look_and_relative_move,
 [], %% One way
 [eid, dx,   dy,   dz,   yaw,  pitch],  %% From Server
 [int, byte, byte, byte, byte, byte],
 16#21},

{%% Entity Teleport 0x22
 entity_teleport,
 [], %% One way
 [eid, x,   y,   z,   yaw,  pitch],  %% From Server
 [int, int, int, int, byte, byte],
 16#22},

{%% Entity Head 0x23
 entity_look_and_relative_move,
 [], %% One way
 [eid, head_yaw],  %% From Server
 [int, byte],
 16#23},

{%% Map Column Allocation 0x32
 map_column_allocation,
 [], %% One way
 [x,   y,   mode], %% From Server
 [int, int, bool],
 16#32},

{%% New Block Type 0x35
 new_block_type,
 [], %% One way
 [x,   y,    z,   block_type, block_metdata], %% From Server
 [int, byte, int, byte,       byte],
 16#35},

{%% Change Game State 0x46
  change_game_state,
  [], %% One way
  [reason, game_mode], %% From Server
  [byte,   byte],
  16#46},

{%% Player Abilities 0xCA
 player_abilities,
 [invulnerability, is_flying, can_fly, instant_destroy], %% To Server
 [invulnerability, is_flying, can_fly, instant_destroy], %% From Server
 [bool,            bool,      bool,    bool],
 16#ca},

{%% Disconnect 0xFF
 disconnect,
 [reason], %% To Server
 [reason], %% From Server
 [string],
 16#ff}

]). %% End 
