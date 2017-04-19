%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    Encode/decode digiplex messages
%%% @end
%%% Created : 21 Oct 2016 by Tony Rogvall <tony@rogvall.se>

-module(digiplex_codec).

-include("digiplex.hrl").

-export([decode_pdu/1, encode_pdu/1]).
-compile(export_all).

encode_pdu(#digiplex_init { address=AA, 
			    eeaddr=EEADDR,
			    product_id=II,
			    software_version=SS,
			    software_revision=RR,
			    software_id=CC,
			    password=WW,
			    module_id=MM,
			    modem_speed=DD,
			    winload_type_id=ID,
			    memory_map_version = E2,
			    event_list_version = EV,
			    firware_build_version=FW,
			    module_serial_number=NN,
			    section_data=DD
			  }) ->
    <<?DIGIPLEX_INIT:4,0:4, AA, EEADDR:16,
      II, SS, RR, CC, WW:16, MM:16, ID, E2, EV, FW:16, NN:32, DD:24>>.


decode_pdu(<<?DIGIPLEX_INIT:4,O:4,
	     AA, EEADDR:16,
	     II, SS, RR, CC, WW:16, MM:16, ID, E2, EV, FW:16, NN:32, DD:24,
	     _Data:12/binary>>) ->
    #digiplex_init { address=AA, 
		     eeaddr=EEADDR,
		     message_center=O,
		     product_id=II,
		     software_version=SS,
		     software_revision=RR,
		     software_id=CC,
		     password=WW,
		     module_id=MM,
		     modem_speed=DD,
		     winload_type_id=ID,
		     memory_map_version = E2,
		     event_list_version = EV,
		     firware_build_version=FW,
		     module_serial_number=NN,
		     section_data=DD
		   }.

add_checksum(Bin) ->
    Size = byte_size(Bin),
    Pad  = 36 - Size,
    Crc = digiplex_crc:checksum(Bin, Pad),
    <<Bin/binary, 0:Pad/unit:8, Crc>>.
