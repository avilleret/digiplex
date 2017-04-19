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

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("digiplex.hrl").

-record(state, 
	{
	  state       :: undefined | init | login | up,
	  device      :: string(),
	  baud        = 19200  :: 19200 | 38400,
	  %% password: 4 bcd digits (hex, in 0-9 range)
	  password    = 16#0000 :: integer(),
	  attempt     = 0,
	  reopen_ival = infinity :: non_neg_integer(),
	  reopen_timer :: undefined | reference(),
	  uart :: port(),
	  buf=(<<>>)
	}).

%%%===================================================================
%%% API
%%%===================================================================

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
		 reopen_ival = Reopen_ival },
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
    case <<(State#state.buf)/binary, Data/binary>> of
	<<Data1:36/binary, Crc, Buf1/binary>> ->
	    case digiplex_crc:checksum(Data1) of
		Crc ->
		    try digiplex_codec:decode_pdu(Data1) of
			Pdu ->
			    State1 = State#state { buf=Buf1},
			    handle_pdu(Pdu, State1)
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
	    send_pdu(Pdu1, State),
	    {noreply, State#state { state = login,
				    attempt = State#state.attempt+1 }}
    end;
handle_pdu(_Pdu=#digiplex_login {}, State) when State#state.state =:= login ->
    lager:debug("login pdu = ~p", [_Pdu]),
    %% initiate read from 16#127
    lager:info("digiplex: read RAM 1",[]),
    Pdu1 = #digiplex_read_command { offset=0,bus_address=0,address=16#8127 },
    send_pdu(Pdu1, State),
    {noreply, State#state { state = read1 }};
handle_pdu(_Pdu=#digiplex_read {}, State) when State#state.state =:= read1 ->
    lager:debug("data @ 0x127 = ~p", [_Pdu]),
    %% read from 16#147
    lager:info("digiplex: read RAM 2",[]),
    Pdu1 = #digiplex_read_command { offset=0,bus_address=0,address=16#8147 },
    send_pdu(Pdu1, State),
    {noreply, State#state { state = read2 }};
handle_pdu(_Pdu=#digiplex_read {}, State) when State#state.state =:= read2 ->
    lager:debug("data @ 0x147 = ~p", [_Pdu]),
    %% read from 16#167
    lager:info("digiplex: read RAM 3",[]),
    Pdu1 = #digiplex_read_command { offset=0,bus_address=0,address=16#8167 },
    send_pdu(Pdu1, State),
    {noreply, State#state { state = read3 }};
handle_pdu(_Pdu = #digiplex_read {}, State) when State#state.state =:= read3 ->
    lager:debug("data @ 0x167 = ~p", [_Pdu]),
    lager:info("digiplex: up",[]),
    {noreply, State#state { state = up }};
handle_pdu(Pdu, State) ->
    lager:debug("got pdu ~p\n", [Pdu]),
    {noreply, State}.

send_pdu(Pdu, State) ->
    lager:debug("send pdu = ~p", [Pdu]),
    Data0 = digiplex_codec:encode_pdu(Pdu),
    Data = digiplex_codec:add_checksum(Data0),
    lager:debug("send data = ~p", [Data]),
    uart:send(State#state.uart, Data).

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
