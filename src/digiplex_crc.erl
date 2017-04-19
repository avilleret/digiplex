%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    CRC algorithm
%%% @end
%%% Created : 21 Oct 2016 by Tony Rogvall <tony@rogvall.se>

-module(digiplex_crc).
-compile(export_all).

checksum(Data) when is_binary(Data) ->
    update_bin(0, Data, 0).

checksum(Data,Pad) when is_binary(Data) ->
    update_bin(0,Data,Pad).

update_bin(Crc, <<Val,Bin/binary>>, Pad) ->
    update_bin(Crc+Val, Bin, Pad);
update_bin(Crc, <<>>, _I) ->
    Crc band 16#ff.

test1() ->
    checksum(<<"123456789">>) =:= 221.

test2() ->
    checksum(<<"Hello, world!">>) =:= 137.
