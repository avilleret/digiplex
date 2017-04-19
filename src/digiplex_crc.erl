%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    CRC algorithm
%%% @end
%%% Created : 21 Oct 2016 by Tony Rogvall <tony@rogvall.se>

-module(digiplex_crc).
-compile(export_all).

checksum(Bin) ->
    checksum_bin(0, Bin).

checksum_bin(Crc, <<Val,Bin/binary>>) ->
    checksum_bin(Crc+Val, Bin);
checksum_bin(Crc, <<>>) ->
    Crc band 16#ff.

test1() ->
    checksum(<<"123456789">>) =:= 221.

test2() ->
    checksum(<<"Hello, world!">>) =:= 137.
