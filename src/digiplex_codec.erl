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
encode_pdu(#digiplex_speed_req { speed=Speed }) ->
    %% Yes. Speed is set twice.
    <<?DIGIPLEX_SPEED:4, 0:4, 0, Speed, Speed>>;
encode_pdu(#digiplex_set_panel_time_req { address=Address,
					  century=Century,
					  year=Year,
					  month=Month,
					  day=Day}) ->
    <<?DIGIPLEX_TIME:4, 0:4,
      Address, 0, 0, Century, Year, Month, Day>>;
encode_pdu(#digiplex_monitor_req {
	      partition1 = P1,
	      partition2 = P2,
	      partition3 = P3,
	      partition4 = P4,
	      partition5 = P5,
	      partition6 = P6,
	      partition7 = P7,
	      partition8 = P8 }) ->
    <<?DIGIPLEX_MONITOR:4, 0:4,
      0, P1:4, P2:4, P3:4, P4:4, P5:4, P6:4, P7:4, P8:4>>;
encode_pdu(#digiplex_read_req { count=Count,
				bus_address=BusAddress,
				address=Address }) 
  when Count > 1, Count =< 32 ->
    <<?DIGIPLEX_READ:4,
      Count:5, BusAddress:7,  %% Count=32 => Count=0
      Address:16>>;
encode_pdu(#digiplex_write_req { count=Count,
				 bus_address=BusAddress,
				 address=Address,
				 data=Data }) ->
    Data1 = iolist_to_binary(Data),
    Size  =  byte_size(Data1),
    Count1 = if  Size =:= 0 -> erlang:error(message_too_small);
		 Count =:= undefined -> Size;
		 Count < 0; Count > 32 -> erlang:error(einval);
		 true -> Count
	     end,
    <<Data2:Count1,_/binary>> = Data1,
    <<?DIGIPLEX_WRITE:4,
      Count:5, BusAddress:7,  %% Count=32 => Count=0
      Address:16,Data2/binary>>;

encode_pdu(#digiplex_event_req {
	      event_request_number = EventRequestNumber
	     }) ->
    <<?DIGIPLEX_EVENT:4,0:4, 0:8, EventRequestNumber:16>>.


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
    #digiplex_login_resp { message_center=O,
			   answer=K,
			   callback=Callback };
decode_pdu(<<?DIGIPLEX_MONITOR:4, O:4,
	     _:8, P1:4, P2:4, P3:4, P4:4, P5:4, P6:4, P7:4, P8:4, _/binary>>) ->
    #digiplex_monitor_resp {
       message_center=O,
       partition1 = P1,
       partition2 = P2,
       partition3 = P3,
       partition4 = P4,
       partition5 = P5,
       partition6 = P6,
       partition7 = P7,
       partition8 = P8 };
decode_pdu(<<?DIGIPLEX_READ:4,O:4,
	     BusAddress:8,
	     Address:16, Data/binary>>) ->
    #digiplex_read_resp { message_center=O,
			  bus_address = BusAddress,
			  address=Address,
			  data=Data };
decode_pdu(<<?DIGIPLEX_WRITE:4,O:4,
	     BusAddress:8,
	     Address:16, _/binary>>) ->
    #digiplex_write_resp { message_center=O,
			   bus_address = BusAddress,
			   address=Address
			 };
decode_pdu(<<?DIGIPLEX_ERROR:4,O:4,
	     Message:8, _/binary>>) ->
    #digiplex_error_resp { message_center=O,
			   message = Message };
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
    #digiplex_event_resp {
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
