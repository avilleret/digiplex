-ifndef(__DIGIPLEX_HRL_).
-define(__DIGIPLEX_HRL_, true).

-define(DIGIPLEX_INIT,         16#0).
-define(DIGIPLEX_LOGIN,        16#1).
-define(DIGIPLEX_SPEED,        16#2).
-define(DIGIPLEX_TIME,         16#3).
-define(DIGIPLEX_MONITOR,      16#4).
-define(DIGIPLEX_READ,         16#5).  %% upload
-define(DIGIPLEX_WRITE,        16#6).  %% download
-define(DIGIPLEX_WRONG_ADRESS, 16#7).  %% response only
-define(DIGIPLEX_SAVE_EVENT,   16#8).
-define(DIGIPLEX_SEND,         16#A).
-define(DIGIPLEX_BROADCAST,    16#B).
-define(DIGIPLEX_UNLOCK,       16#C).
-define(DIGIPLEX_ZONE_CHANGE,  16#D).
-define(DIGIPLEX_EVENT,        16#E).

-define(EEPROM(A), ((A) band 16#7fff)).
-define(RAM(A),    (((A) band 16#7fff) bor 16#8000)).

%% RAM memory map
%%  *127
%%   13C - Digiplex 096 zone status base address
%%  *147
%%   148 - Digiplex 096 tamper status base address
%%   153 - 1  .. 8 Digiplex 048 zone status base address
%%   154 - 9  .. 16
%%   155 - 17 .. 24
%%   156 - 25 .. 32
%%   157 - 33 .. 40
%%   158 - 41 .. 48
%%   159 - 1  .. 8 Digiplex 048 tamper status base address
%%   15A - 9  .. 16
%%   15B - 17 .. 24
%%   15C - 25 .. 32
%%   15D - 33 .. 40
%%   15E - 41 .. 48
%%  *167
%%  

-define(DIGIPLEX_VERSION_1_30, 16#00).
-define(DIGIPLEX_VERSION_2_00, 16#01).
-define(DIGIPLEX_VERSION_NE,   16#02).

-define(DIGIPLEX_ENDUSER_TYPE, 16#55).

-define(DIGIPLEX_PRODUCT_ID_DIGIPLEX, 16#00).
-define(DIGIPLEX_PRODUCT_ID_SPECTRA,  16#10).
-define(DIGIPLEX_PRODUCT_ID_CONTACT,  16#20).


%% req/resp: 0 
-record(digiplex_init,
	{ 
	  address=0, 
	  eeaddr=0,
	  message_center,
	  product_id=0,
	  software_version=0,
	  software_revision=0,
	  software_id=0,
	  password=0,
	  module_id=0,
	  modem_speed=0,
	  winload_type_id=?DIGIPLEX_ENDUSER_TYPE,
	  memory_map_version,
	  event_list_version,
	  firware_build_version,
	  module_serial_number,
	  section_data
	}).

%% resp: 1
-record(digiplex_login_resp,
	{ 
	  message_center,
	  answer,
	  callback 
	}).

%% req: 2
-record(digiplex_speed_req,
	{
	  speed=0     %% byte code?
	}).

%% resp: 2
-record(digiplex_speed_resp,
	{
	  message_center,	  
	  speed=0     %% byte code?
	}).
%% Set panel time (may should be called set_panel_date?)
%% req: 3 
%% BCD?
-record(digiplex_set_panel_time_req, 
	{
	  address=0,
	  century,
	  year,
	  month,
	  day
	}).

%% Monitoring command
%% req: 4
%%
-define(DIGIPLEX_NOP,          16#0).
-define(DIGIPLEX_FULL_ARM,     16#2).
-define(DIGIPLEX_STAY_ARM,     16#3).
-define(DIGIPLEX_INSTANT_ARM,  16#4).
-define(DIGIPLEX_FORCE_ARM,    16#5).
-define(DIGIPLEX_DISARM,       16#6).
-define(DIGIPLEX_BEEP,         16#8).

-record(digiplex_monitor_req,
	{
	  partition1,
	  partition2,
	  partition3,
	  partition4,
	  partition5,
	  partition6,
	  partition7,
	  partition8
	}).

-record(digiplex_monitor_resp,
	{
	  message_center,
	  partition1,
	  partition2,
	  partition3,
	  partition4,
	  partition5,
	  partition6,
	  partition7,
	  partition8
	}).
%%
%% req: 5
%%
-record(digiplex_read_req,
	{
	  count,  %% :5  0..31  0=32 bytes
	  bus_address = 0,  %% :7 bits
	  address  %% :16 16#8000 + addr = RAM, 16#0000 +addr = EEPROM
	}).
%%
%% resp: 5
%%
-record(digiplex_read_resp,
	{
	  message_center,
	  bus_address,
	  address,
	  data
	}).

%%
%% req: 6
%%
-record(digiplex_write_req,
	{
	  count,  %% :5  0..31  0=32 bytes
	  bus_address = 0,  %% :7 bits
	  address,  %% :16 16#8000 + addr = RAM, 16#0000 +addr = EEPROM
	  data :: binary() %% 1..32 bytes
	}).
%%
%% resp: 6
%%
-record(digiplex_write_resp,
	{
	  message_center,
	  bus_address,
	  address
	}).
%%
%% req: E
%%
-record(digiplex_event_req,
	{
	  event_request_number
	}).
%%
%% resp: E
%%
-record(digiplex_event_resp,
	{
	  message_center,
	  event_request_number,
	  timestamp,
	  event_group,
	  partition1,     %% 4/8?
	  partition2,     %% 4/8?
	  event_number1, %% 8/integer 12?
	  event_number2, %% 8/integer
	  serial,        %% 32/integer
	  event_data     %% 16/binary
	}).

-endif.
