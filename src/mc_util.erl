%%%-----------------------------------------------------------------------------
%%% Use is subject to License terms.
%%% @copyright (C) 2012 Our Robot Shop, Inc.
%%% @doc Module to repsent a Minecraft Client.
%%% @end
%%%-----------------------------------------------------------------------------

-module(mc_util).
-author("Our Robot Shop, Inc.").

%-include("../include/minecraft_client.hrl").

-compile([export_all]).

abint2float(AbInt) when is_integer(AbInt) ->
    case AbInt >= 16#40000000 of
	true ->
	    Int = 0-(16#FFFFFFFF - AbInt);
	false -> 
	    Int = AbInt
    end,
    Float = Int / 32.0,
    Float.
    
