%%%-----------------------------------------------------------------------------
%%% Use is subject to License terms.
%%% @copyright (C) 2012 Our Robot Shop, Inc.
%%% @doc Module for encoding and decoding Entity Metadata.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mcp_entity_md).
-author("Our Robot Shop, Inc.").

-include("../include/mcp.hrl").
-compile([export_all]).

%%% @doc Entity Metadata Format
%%%
%%% Note that entity metadata is a totally distinct concept from block metadata.
%%% The entity metadata format is quirky dictionary format, where the key 
%%% and the value's type are packed in a single byte.
%%% To parse, repeat the following procedure:
%%% 1) Read an unsigned byte
%%% 2) If this byte == 127, stop reading
%%% 3) Decompose the byte. 
%%% The bottom 5 bits (0x1F) serve as an identifier (key) for the data to follow. 
%%% The top 3 bits (0xE0) serve as a type:
%%% 0: byte
%%% 1: short
%%% 2: int
%%% 3: float
%%% 4: string16,
%%% 5: short, byte, short (slot type)
%%% 6: int, int, int
%%% 4) Read and unpack based on the type (above)
%%%
%%% Find more info here: http://www.wiki.vg/Entities
%%%
%%% @end
%%%-----------------------------------------------------------------------------

%% Type Coding
-define(MCP_MD_BYTE,0).
-define(MCP_MD_SHORT,1).
-define(MCP_MD_INT,2).
-define(MCP_MD_FLOAT,3).
-define(MCP_MD_STRING16,4).
-define(MCP_MD_SLOT,5).  %% short, byte, short
-define(MCP_MD_THREE_INT,6).  %% int, int, int


header(Header) when is_binary(Header) ->
    <<Type:3,Key:5>> = Header,
    get_type(Type).


get_type(Type) when is_integer(Type),
		    Type >= ?MCP_MD_BYTE,
		    Type =< ?MCP_MD_THREE_INT ->
    case Type of
	?MCP_MD_BYTE ->
	    byte;
	?MCP_MD_SHORT ->
	    short;
	?MCP_MD_INT ->
	    int;
	?MCP_MD_FLOAT ->
	    float;
	?MCP_MD_STRING16 ->
	    string16;
	?MCP_MD_SLOT ->
	    md_slot;
	?MCP_MD_THREE_INT ->
	    three_int
    end;
get_type(Type) when is_atom(Type)->
    case Type of
	byte -> 
	    ?MCP_MD_BYTE;
	short -> 
	    ?MCP_MD_SHORT;
	int -> 
	    ?MCP_MD_INT;
	float -> 
	    ?MCP_MD_FLOAT;
	string16 -> 
	    ?MCP_MD_STRING16;
	md_slot -> 
	    ?MCP_MD_SLOT;
	three_int ->
	    ?MCP_MD_THREE_INT
    end.


set_type(Type,Header) when is_atom(Type) ->
    NewType = get_type(Type), %% convert type from atom() to integer().
    set_type(NewType,Header);
set_type(Type,Header) when is_integer(Type),
			   is_binary(Header),
			   Type >= ?MCP_MD_BYTE,
			   Type =< ?MCP_MD_THREE_INT ->
    <<_OldType:3,Key:5>> = Header,
    NewHeader = <<Type:3,Key:5>>,
    NewHeader.


