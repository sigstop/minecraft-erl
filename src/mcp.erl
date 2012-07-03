%%%-----------------------------------------------------------------------------
%%% Use is subject to License terms.
%%% @copyright (C) 2012 Our Robot Shop, Inc.
%%% @doc Module for the Minecraft Protocol.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mcp).
-author("Our Robot Shop, Inc.").

-include("../include/mcp.hrl").

-compile(export_all).


%% Encodings
-define(NATIVE_ENCODING,utf8).
-define(MCP_ENCODING,utf16).
-define(MCP_STRLEN_BITS,16).
-define(MCP_BYTES_PER_CHAR,2).
-define(MCP_BOOL_BITS,8).
-define(MCP_SHORT_BITS,16).
-define(MCP_INT_BITS,32).
-define(MCP_LONG_BITS,64).
-define(MCP_FLOAT_BITS,32).
-define(MCP_DOUBLE_BITS,64).
-define(MCP_BYTE_BITS,7).
-define(MCP_SIGN_BIT,1).
-define(MCP_UBYTE_BITS,8).
-define(MCP_MSG_TYPE_BITS,8).

create_values(ID)->
    SchemaLength = get_schema_length(ID),
    Values = lists:duplicate(SchemaLength,empty),
    Values.
    

write_packet(Message = {ID, ValueTuples})->
    io:format("To Server:~n~p~n",[Message]),
    Values = set_values( {ID, ValueTuples}, encode),
    Packet = encode( ID, Values),
%    io:format("To Server (Binary):~n~p~n",[Packet]),
    Packet.

read_packet(Packet)->
    Messages = read_packet(Packet,[]),
    Messages.


read_packet(Packet, Messages)->
    case Packet of
	<<>> -> 
	    Messages;
	_ ->
	    try
%		io:format("From Server (Binary):~n~p~n",[Packet]),
		<<IDNum:?MCP_MSG_TYPE_BITS,MCBinary/binary>> = Packet,
		{{IDNum, ValueTuples},{remainder,Remainder}} = decode(IDNum, MCBinary),
		ID = get_schema_name(IDNum),
		Message = {ID, ValueTuples},
%		io:format("From Server:~n~p~n",[Message]),
		NewMessages = lists:append(Messages,[Message]),
		read_packet(Remainder,NewMessages)
	    catch
		Error -> {Messages,Error,{remainder,Packet}}
	    end
    end.



set_values({ID, ValueTuples},Direction)->
    EmptyValues = create_values(ID),
    Values = set_values({ID,ValueTuples},Direction,EmptyValues),
    Values.

set_values({ID, ValueTuples}, Direction, Values ) when is_integer(ID);
						       is_atom(ID) ->
    Schema = get_schema(ID),
    set_values({Schema, ValueTuples}, Direction, Values);
set_values( {Schema,ValueTuples}, Direction, Values) when is_tuple(Schema)->
    case ValueTuples of
	[]->Values;
	[{Name,Value}|RemainingValueTuples]->
	    Position = get_value_pos(Schema, Name, Direction),
	    NewValues = set_value_at_pos(Position,Value,Values),
	    set_values({Schema, RemainingValueTuples}, Direction, NewValues)
    end.

get_value_pos(ID, Name, Direction) when is_tuple(ID) == false,
					is_atom(Name),
					is_atom(Direction)->
    Schema = get_schema(ID),
    get_value_pos(Schema, Name, Direction);
get_value_pos(Schema, Name, Direction) when is_tuple(Schema),
					    is_atom(Name),
					    is_atom(Direction)->
    case Direction of
	encode ->
	    {_Name,Fields,_Decode,_CodecTypes,_Number}=Schema;
	decode ->
	    {_Name,_Encode,Fields,_CodecTypes,_Number}=Schema
    end,
    get_value_pos(Name,Fields,0);
get_value_pos(Name,Fields,Position) when is_atom(Name),
					 Fields == [],
					 is_integer(Position)->
    error;
get_value_pos(Name,Fields,Position) when is_atom(Name),
					 is_list(Fields),
					 is_integer(Position)->
    [Head|RemainingFields]=Fields,
    case Name == Head of
	true -> Position;
	false -> get_value_pos(Name,RemainingFields,Position+1)
    end.  
    

	    
set_value_at_pos(Position,Value,Values)->
    {Head,Tail} = lists:split(Position,Values),
    NewHead = lists:append([Head,[Value]]),
    [_Discard|NewTail]=Tail,
    NewValues = lists:append([NewHead,NewTail]),
    NewValues.


decode(ID, ValuesBin) when is_atom(ID) ->
    NumID = get_schema_num(ID),
    decode( NumID, ValuesBin);
decode( ID, MCBinary ) when is_integer(ID)->
    SchemaLength = get_schema_length(ID),
    {Values, Remainder} = priv_decode( ID, MCBinary, 0, SchemaLength, []),
    {{ID, Values},{remainder, Remainder}}.

priv_decode( ID, MCBinary, Pos, SchemaLength, Values )->
    case Pos == SchemaLength of
	true ->
	    {Values, MCBinary};
	false -> {Value, NewMCBinary} = decode_value(ID,Pos,MCBinary),
	     Name = get_decode_name(ID,Pos),
	     NewValues = lists:append(Values,[{Name,Value}]),
	     priv_decode(ID, NewMCBinary, Pos+1, SchemaLength, NewValues)
    end.

decode_value(ID,Pos,MCBinary)->
    CodecType = get_codec_type(ID,Pos), 
    ValueBinary = ?MODULE:CodecType(decode,MCBinary),
    ValueBinary.
	   
    
encode(ID, Values) when is_atom(ID) ->
    NumID = get_schema_num(ID),   
    encode( NumID, Values);
encode( ID, Values ) when is_integer(ID)->
    Packet = priv_encode( ID, Values, 0, <<ID>>),
    Packet.

priv_encode( ID, Values, Pos, MCBinary )->
    case Values of
	[]->
	    MCBinary;
	[Value|RemainingValues] ->
	    ValueBinary = encode_value(ID,Pos,Value),
	    NewMCBinary = <<MCBinary/binary,ValueBinary/binary>>,
	    priv_encode(ID,RemainingValues,Pos+1,NewMCBinary)
    end.

encode_value(ID,Pos,Value)->
    CodecType = get_codec_type(ID,Pos), 
    ValueBinary = ?MODULE:CodecType(encode,Value),
    ValueBinary.

bool(Direction,Value) ->
    case Direction of
	encode when Value == empty; Value == false; Value == 0 ->
	    <<0:?MCP_BOOL_BITS>>;
	encode when Value == true; Value == 1 ->
	    <<1:?MCP_BOOL_BITS>>;
	decode when is_binary(Value) ->
	    <<NumberValue:?MCP_BOOL_BITS/integer,Remainder/binary>> = Value,
	    case NumberValue of
		0 -> BoolValue = false;
		1 -> BoolValue = true
	    end,
	    {BoolValue,Remainder}
    end.
  

integer_number(Direction,Value,Bits) ->
    case Direction of
	encode when Value == empty ->
	    <<0:Bits>>;
	encode when is_integer(Value) ->
	    <<Value:Bits/integer>>;
	decode when is_binary(Value) ->
	    <<NumberValue:Bits/integer,Remainder/binary>>=Value,
	    {NumberValue,Remainder}
    end.

float_number(Direction,Value,Bits) ->
    case Direction of
	encode when Value == empty ->
	    <<0:Bits>>;
	encode when is_float(Value) ->
	    <<Value:Bits/float>>;
	decode when is_binary(Value) ->
	    <<NumberValue:Bits/float,Remainder/binary>>=Value,
	    {NumberValue,Remainder}
    end.
    

short(Direction,Value)->
    integer_number(Direction,Value,?MCP_SHORT_BITS).

int(Direction,Value)->
    integer_number(Direction,Value,?MCP_INT_BITS).

three_int(Direction,Value)->
    ok.

long(Direction,Value)->
    integer_number(Direction,Value,?MCP_LONG_BITS).

float(Direction,Value)->
    float_number(Direction,Value,?MCP_FLOAT_BITS).

double(Direction,Value)->
    float_number(Direction,Value,?MCP_DOUBLE_BITS).

string(Direction,Value)->
    case Direction of
	encode when Value == empty ->
	    string_to_mc_binary("");
	encode when is_list(Value) ->
	    string_to_mc_binary(Value);
	decode when is_binary(Value) ->
	    mc_binary_to_string(Value)			       		
    end.

string16(Direction,Value) ->
    ok.

byte(Direction,Value) ->
    case Direction of
	encode when Value == empty ->
	    <<0:?MCP_SIGN_BIT,0:?MCP_BYTE_BITS>>;
	encode when is_integer(Value), 
		    Value > -128, 
		    Value < 128->
	    case Value < 0 of
		true ->
		    AbsValue = 0 - Value,
		    <<1:?MCP_SIGN_BIT,AbsValue:?MCP_BYTE_BITS>>;
		false ->
		    <<0:?MCP_SIGN_BIT,Value:?MCP_BYTE_BITS>>
	    end;
	decode when is_binary(Value) ->
	    <<Sign:?MCP_SIGN_BIT,AbsValue:?MCP_BYTE_BITS,Remainder/binary>> = Value,
	    case Sign == 1 of
		true -> 
		    ByteValue = 0-AbsValue;
		false ->
		    ByteValue = AbsValue
	    end,
	    {ByteValue,Remainder}
    end.

ubyte(Direction,Value)->
    case Direction of
	encode when Value == empty ->
	    <<0:?MCP_UBYTE_BITS>>;
	encode when is_integer(Value), Value >= 0, Value < 256 ->
	    <<Value:?MCP_UBYTE_BITS>>;
	decode when is_binary(Value) ->
	    <<UIntValue:?MCP_UBYTE_BITS,Remainder/binary>> = Value,
	    {UIntValue,Remainder}
    end.

slot(Direction,Value) ->
    ok.


string_to_mc_binary(String)->
    Length = string:len(String),
    LengthBin = <<Length:?MCP_STRLEN_BITS>>,
    case Length of
	0 -> MCBinary = LengthBin;
	_ -> StringBin = unicode:characters_to_binary(String,?NATIVE_ENCODING,?MCP_ENCODING),
	     MCBinary = <<LengthBin/binary,StringBin/binary>>
    end,
    MCBinary.


mc_binary_to_string(MCBinary)->
    <<StrLength:?MCP_STRLEN_BITS,StringRemainder/binary>> = MCBinary,
    Length = StrLength * ?MCP_BYTES_PER_CHAR,
    case StringRemainder of
	<<String:Length/binary,Remainder/binary>> ->
	    StringBin = unicode:characters_to_binary(String,?MCP_ENCODING,?NATIVE_ENCODING),
	    Value = binary:bin_to_list(StringBin),
	    {Value,Remainder};
	_ -> throw({parse_error,MCBinary})
    end.
    

    
	    
%% Utilities
mc_protocol()->
    ?MC_PROTO.

get_schema(ID) when is_integer(ID) ->
    Find = fun(Element)->
		   {_,_,_,_,Number} = Element,
		   Number == ID
	   end,
    Schemas = lists:filter(Find,mc_protocol()),
    case Schemas of
	[] -> throw({no_schema_type,ID});
	[Schema] -> Schema
    end;
get_schema(ID) when is_atom(ID) ->
    Schema = lists:keyfind(ID,?MC_PROTO_PACKET_TYPE_POS,mc_protocol()),
    Schema.

get_schema_name(ID)->
    {Name,_Encode,_Decode,_CodecTypes,_Number}=get_schema(ID),
    Name.

get_schema_num(ID) when is_atom(ID)->
    {_Name,_Encode,_Decode,_CodecTypes,Number}=get_schema(ID),
    Number.




get_schema_length(ID)->
    {_Name,_Encode,_Decode,CodecTypes,_Number}=get_schema(ID),
    Length = length(CodecTypes),
    Length.


get_types(ID)->
    {_Name,_Encode,_Decode,CodecTypes,_Number}=get_schema(ID),
    CodecTypes.

get_encode_names(ID)->
    {_Name,Encode,_Decode,_CodecTypes,_Number}=get_schema(ID),
    Encode.

get_decode_names(ID)->
    {_Name,_Encode,Decode,_CodecTypes,_Number}=get_schema(ID),
    Decode.
    
get_codec_type(ID,Pos)->
    Types = get_types(ID),
    CodecType = lists:nth(Pos+?MC_PROTO_VALUE_POS_OFFSET,Types),
    CodecType.
    

get_encode_name(ID,Pos)->
    EncodeNames = get_encode_names(ID),
    EncodeName = lists:nth(Pos+?MC_PROTO_VALUE_POS_OFFSET,EncodeNames),
    EncodeName.

get_decode_name(ID,Pos)->
    DecodeNames = get_decode_names(ID),
    DecodeName = lists:nth(Pos+?MC_PROTO_VALUE_POS_OFFSET,DecodeNames),
    DecodeName.
    
	    

