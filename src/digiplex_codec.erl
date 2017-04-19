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
      II, SS, RR, CC, WW:16, MM:16, ID, E2, EV, FW:16, NN:32, DD:24>>;
encode_pdu(#digiplex_read_command { offset=Offset,
				    bus_address=BusAddress,
				    address=Address }) ->
    <<?DIGIPLEX_READ:4,
      Offset:5, BusAddress:7,
      Address:16>>.

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
		   };
decode_pdu(<<?DIGIPLEX_LOGIN:4,O:4,
	     K:4, _:4, Callback:16,_Data/binary>>) ->
    #digiplex_login { message_center=O,
		      answer=K,
		      callback=Callback };
decode_pdu(<<?DIGIPLEX_READ:4,O:4,
	     BusAddress:8,
	     Address:16, Data/binary>>) ->
    #digiplex_read { message_center=O,
		     bus_address = BusAddress,
		     address=Address,
		     data=Data };
decode_pdu(<<?DIGIPLEX_EVENT:4,O:4,
	     EventRequestNumber : 8,
	     Century:8, Year:8, Month:8, Day:8,
	     Hour:8, Minute:8, 
	     EventGroup: 8,
	     Partition1:4, Partition2:4,
	     EventNumber1:8,
	     EventNumber2:8,
	     Serial:32,
	     EventData/binary>>) ->
    #digiplex_event {
       message_center = O,
       event_request_number = EventRequestNumber,
       timestamp = {{ Century*100+Year, Month, Day},{Hour,Minute,0}},
       event_group = EventGroup,
       partition1 = Partition1,
       partition2 = Partition2,
       event_number1 = EventNumber1,
       event_number2 = EventNumber2,
       serial = Serial,
       event_data = EventData }.


add_checksum(Bin) ->
    Size = byte_size(Bin),
    Pad  = 36 - Size,
    Crc = digiplex_crc:checksum(Bin),
    <<Bin/binary, 0:Pad/unit:8, Crc>>.
