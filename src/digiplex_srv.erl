%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2016, Tony Rogvall
%%% @doc
%%%    Paradox digiplex protocol
%%% @end
%%% Created : 20 Oct 2016 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(digiplex_srv).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_zone_info/0]).
-export([get_info/0]).
-export([show_zone_info/0]).
-export([read_event/1]).
-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("digiplex.hrl").

%% info from init rsponse
-record(info,
	{
	  product_id,
	  software_version,
	  software_revision,
	  software_id,
	  modem_speed,
	  winload_type_id,
	  memory_map_version,
	  event_list_version,
	  firware_build_version,
	  module_serial_number
	}).

-define(MEMMAP_BEGIN, 16#127).
-define(MEMMAP_END,   16#167).
-define(MEMMAP_SIZE,  512).
%%-define(MEMMAP_BEGIN, 16#0).
%%-define(MEMMAP_END,   (?MEMMAP_SIZE-32)).

-record(state,
	{
	  state       :: undefined | init | login | read | up,
	  device      :: string(),
	  baud        = 19200  :: 19200 | 38400,
	  %% password: 4 bcd digits (hex, in 0-9 range)
	  password    = 16#0000 :: integer(),
	  attempt     = 0,
	  info        :: #info{},
	  reopen_ival = infinity :: non_neg_integer(),
	  reopen_timer :: undefined | reference(),
	  uart :: port(),
	  read_addr,  %% pening read address
	  memory_map :: binary(), %% RAM map
	  sent_pdu,   %% last sent pdu
	  wait_pdu,   %% {pdu_name, From, Data}
	  buf=(<<>>)
	}).

%%%===================================================================
%%% API
%%%===================================================================

show_zone_info() ->
    {ok,ZoneInfo} = get_zone_info(),
    lists:foreach(
      fun({_I,ok}) -> %% ignore
	      ok;
	 ({I,Info}) -> 
	      io:format("~w: ~w\n", [I, Info])
      end, ZoneInfo).

get_memory_map() ->
    gen_server:call(?SERVER, get_memory_map).

get_zone_info() ->
    {ok, MemoryMap} = get_memory_map(),
    %% Digiplex 48?
    <<_:16#152/binary, ZoneBin:6/binary, TamperBin:6/binary, 
      _/binary>> = MemoryMap,
    ZoneStatus = bit_info(ZoneBin),
    TamperStatus = bit_info(TamperBin),
    {ok,[{I, case Z*2+T of
		 0 -> ok;
		 1 -> tamper;
		 2 -> open;
		 3 -> fire_loop
	     end} || {I,Z,T} <- lists:zip3(lists:seq(1,48),
					   ZoneStatus,TamperStatus)]}.

bit_info(<<Byte,Bytes/binary>>) ->
    lists:reverse([I || <<I:1>> <= <<Byte>>]) ++ bit_info(Bytes);
bit_info(<<>>) ->
    [].

get_info() ->
    gen_server:call(?SERVER, get_info).

read_event(N) ->
    gen_server:call(?SERVER, {read_event,N}).

read_ram(Addr, Count) ->
    case gen_server:call(?SERVER, {read_memory,?RAM(Addr), Count}) of
	{ok,Pdu} ->
	    {ok,Pdu#digiplex_read_resp.data};
	Error ->
	    Error
    end.

read_eeprom(Addr, Count) ->
    case gen_server:call(?SERVER, {read_memory,?EEPROM(Addr), Count}) of
	{ok,Pdu} ->
	    {ok,Pdu#digiplex_read_resp.data};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Args0) ->
    Args = Args0 ++ application:get_all_env(digiplex),
    Device   = proplists:get_value(device, Args),
    Baud     = proplists:get_value(baud, Args, 19200),
    Password = case proplists:get_value(password, Args) of
		   P when is_list(P) -> erlang:list_to_integer(P, 16);
		   P when is_integer(P) -> P
	       end,
    Reopen_ival = proplists:get_value(reopen_timeout, Args, infinity),
    S = #state { device = Device,
		 baud = Baud,
		 password = Password,
		 reopen_ival = Reopen_ival,
		 memory_map = <<0:?MEMMAP_SIZE/unit:8>>   %% empty memory map
	       },
    case open(S) of
	{ok, S1} -> {ok,S1};
	Error -> {stop, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call(get_memory_map, _From, State) ->
    {reply, {ok, State#state.memory_map}, State};
handle_call(get_info, _From, State) ->
    I = State#state.info,
    L = [{product_id,I#info.product_id},
	 {software_version,I#info.software_version},
	 {software_revision,I#info.software_revision},
	 {software_id,I#info.software_id},
	 {modem_speed,I#info.modem_speed},
	 {winload_type_id,I#info.winload_type_id},
	 {memory_map_version,I#info.memory_map_version},
	 {event_list_version,I#info.event_list_version},
	 {firware_build_version,I#info.firware_build_version},
	 {module_serial_number,I#info.module_serial_number}],
    {reply, {ok,L}, State};
handle_call({read_event, N}, From, State) ->
    if State#state.state =/= up ->
	    {reply, {error, not_running}, State};
       true ->
	    Pdu = #digiplex_event_req { event_request_number = N },
	    State1 = send_pdu(Pdu, State),
	    Pos = #digiplex_event_resp.event_request_number,
	    Wait = {digiplex_event_resp,From,{Pos,N}},
	    {noreply, State1#state { wait_pdu = Wait }}
    end;
handle_call({read_memory,Addr,Count}, From, State) ->
    if State#state.state =/= up ->
	    {reply, {error, not_running}, State};
       true ->
	    Pdu = #digiplex_read_req { count=Count,
				       bus_address=0,
				       address=Addr },
	    State1 = send_pdu(Pdu, State),
	    Pos = #digiplex_read_resp.address,
	    Wait = {digiplex_read_resp,From,{Pos,Addr}},
	    {noreply, State1#state { wait_pdu = Wait }}
    end;
handle_call(_Request, _From, State) ->
    lager:debug("got call ~p", [_Request]),
    {reply, {error,bad_call}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    lager:debug("got cast ~p", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_info({uart,U,Data}, State) when U =:= State#state.uart ->
    %% lager:debug("got data: ~p", [Data]),    
    case <<(State#state.buf)/binary, Data/binary>> of
	<<Data1:36/binary, Crc, Buf1/binary>> ->
	    case digiplex_crc:checksum(Data1) of
		Crc ->
		    %% lager:debug("got message: ~p", [Data1]),
		    try digiplex_codec:decode_pdu(Data1) of
			Pdu ->
			    lager:debug("got pdu: ~p", [Pdu]),
			    State1 = State#state { buf=Buf1},
			    State2 = preprocess_pdu(Pdu, State1),
			    handle_pdu(Pdu, State2)
		    catch
			error:Error ->
			    lager:debug("panel data: ~p", [Data1]),
			    lager:error("decode error: ~p", [Error]),
			    {noreply,State#state { buf=Buf1}}
		    end;
		_Crc0 ->
		    lager:error("bad crc ~w error: data=~p, crc=~p", 
				[_Crc0,Data1,Crc]),
		    {noreply,State#state { buf=Buf1}}
	    end;
	Buf1 ->
	    {noreply,State#state { buf=Buf1}}
    end;
handle_info({timeout,TRef,reopen}, State) when
      State#state.reopen_timer =:= TRef ->
    case open(State#state { reopen_timer = undefined }) of
	{ok, State1} -> {noreply, State1};
	Error -> {stop, Error, State}
    end;
handle_info(_Info, State) ->
    lager:debug("got info ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

preprocess_pdu(Pdu = #digiplex_init {}, State) ->
    set_info(Pdu, State);
preprocess_pdu(Pdu=#digiplex_read_resp {}, State) ->
    State1 = set_memory_map(Pdu, State),
    resp_pdu(Pdu, State1);
preprocess_pdu(Pdu, State) ->
    resp_pdu(Pdu, State).

%% check for response pdu
resp_pdu(Pdu=#digiplex_error_resp {}, State) ->
    case State#state.wait_pdu of
	undefined -> State;
	{_PduName,From,_Match} ->
	    gen_server:reply(From, {error, Pdu#digiplex_error_resp.message}),
	    State#state { wait_pdu = undefined }
    end;
resp_pdu(Pdu, State) ->
    case State#state.wait_pdu of
	undefined -> State;
	{PduName,From,{Pos,Value}} when 
	      PduName =:= element(1,Pdu),
	      Value =:= element(Pos,Pdu) ->
	    gen_server:reply(From, {ok, Pdu}),
	    State#state { wait_pdu = undefined };
	_ ->
	    State
    end.

handle_pdu(Pdu = #digiplex_init { }, State) when State#state.state =:= init;
						 State#state.state =:= login ->
    lager:debug("init pdu = ~p", [Pdu]),
    if State#state.attempt > 4 ->
	    uart:close(State#state.uart),
	    {ok,State1} = open(State),
	    {noreply, State1};
       true ->
	    lager:info("digiplex: logging in",[]),
	    Pdu1 = Pdu#digiplex_init { password = State#state.password },
	    State1 = send_pdu(Pdu1, State),
	    {noreply, State1#state { state = login,
				     attempt = State#state.attempt+1 }}
    end;
handle_pdu(_Pdu=#digiplex_login_resp {}, State)
  when State#state.state =:= login ->
    lager:debug("login pdu = ~p", [_Pdu]),
    Addr = ?MEMMAP_BEGIN,
    lager:info("digiplex: read RAM addr=~p",[Addr]),
    Pdu1 = #digiplex_read_req { count=32,bus_address=0,
				address=?RAM(Addr) },
    State1 = send_pdu(Pdu1, State),
    {noreply, State1#state { read_addr=Addr,state = read }};

handle_pdu(_Pdu=#digiplex_read_resp {}, State)
  when State#state.state =:= read ->
    lager:info("digiplex: DATA at address ~p = ~p",
	       [State#state.read_addr, _Pdu#digiplex_read_resp.data]),
    if State#state.read_addr >= ?MEMMAP_END ->
	    lager:info("digiplex: up",[]),
	    {noreply, State#state { state = up }};
       true ->
	    Addr = State#state.read_addr + 32, %% Next address
	    lager:info("digiplex: read RAM addr=~p",[Addr]),
	    Pdu1 = #digiplex_read_req { count=32,bus_address=0,
					address=?RAM(Addr) },
	    State1 = send_pdu(Pdu1, State),
	    {noreply, State1#state { read_addr = Addr, state = read }}
    end;

handle_pdu(Pdu, State) ->
    lager:debug("got pdu ~p\n", [Pdu]),
    {noreply, State}.

send_pdu(Pdu, State) ->
    lager:debug("send pdu = ~p", [Pdu]),
    Data0 = digiplex_codec:encode_pdu(Pdu),
    Data = digiplex_codec:add_checksum(Data0),
    lager:debug("send data = ~p", [Data]),
    uart:send(State#state.uart, Data),
    %% fixme: in case of error or crc error retransmit pdu?
    State#state { sent_pdu = Pdu }.


set_info(Pdu, State) ->
    Info = #info { 
	      product_id = Pdu#digiplex_init.product_id,
	      software_version = Pdu#digiplex_init.software_version,
	      software_revision  = Pdu#digiplex_init.software_revision,
	      software_id  = Pdu#digiplex_init.software_id,
	      modem_speed  = Pdu#digiplex_init.modem_speed,
	      winload_type_id = Pdu#digiplex_init.winload_type_id,
	      memory_map_version = Pdu#digiplex_init.memory_map_version,
	      event_list_version = Pdu#digiplex_init.event_list_version,
	      firware_build_version = Pdu#digiplex_init.firware_build_version,
	      module_serial_number = Pdu#digiplex_init.module_serial_number
	     },
    State#state { info = Info }.

%% insert RAM memory to memory map in address range 0-511
set_memory_map(Pdu, State) ->
    if Pdu#digiplex_read_resp.address band 16#8000 =:= 16#8000 ->
	    Addr = Pdu#digiplex_read_resp.address band 16#7fff,
	    Data = Pdu#digiplex_read_resp.data,
	    Size = byte_size(Data),
	    if Addr < (?MEMMAP_SIZE-32) ->
		    lager:debug("set memory map, address ~w data=~w",
				[Addr, Data]),
		    <<B0:Addr/binary, _:Size/binary, B1/binary>> =
			State#state.memory_map,
		    MemoryMap = <<B0/binary, Data/binary, B1/binary>>,
		    State#state { memory_map = MemoryMap };
	       true ->
		    State
	    end;
       true ->
	    State
    end.

open(State = #state {device = DeviceName,
		     baud = Baud,
		     reopen_ival = Reopen_ival }) ->
    UartOpts = [{baud,Baud},{mode,binary},{active,true},
		{csize,8},{parity,none},{stopb,1}],
    case uart:open(DeviceName,UartOpts) of
	{ok,U} ->
	    lager:debug("digiplex open: ~s@~w -> ~p", [DeviceName,Baud,U]),
	    flush(U),
	    lager:debug("send init string", []),
	    uart:send(U, init_string()),
	    {ok, State#state { uart=U, state=init, attempt = 0 }};
	{error, E} when E =:= eaccess;
			E =:= enoent ->
	    if Reopen_ival =:= infinity ->
		    lager:error("open: error = ~p.", [E]),
		    {error, E};
	       true ->
		    lager:debug("open: uart could not be opened, will try again"
				" in ~p millisecs.", [Reopen_ival]),
		    Reopen_timer = erlang:start_timer(Reopen_ival,
						      self(), reopen),
		    {ok, State#state { reopen_timer = Reopen_timer }}
	    end;
	    
	Error ->
	    lager:error("open: error = ~p.", [Error]),
	    Error
    end.

flush(U) ->
    uart:flush(U, both),
    flush_loop(U).

flush_loop(U) ->
    receive
	{uart, U, Data} ->
	    lager:debug("flushed data = ~p", [Data]),
	    flush_loop(U)
    after 1000 ->
	    ok
    end.

init_string() ->
    <<16#5F, 16#20, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 
      16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 
      16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 
      16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 
      16#00, 16#00, 16#00, 16#00, 16#7F>>.
