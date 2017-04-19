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

-define(DIGIPLEX_VERSION_1_30, 16#00).
-define(DIGIPLEX_VERSION_2_00, 16#01).
-define(DIGIPLEX_VERSION_NE,   16#02).

-define(DIGIPLEX_ENDUSER_TYPE, 16#55).

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

-record(digiplex_login,
	{ 
	  message_center,
	  answer,
	  callback 
	}).

-record(digiplex_speed,
	{
	  speed=0
	}).

-record(digiplex_read,
	{
	  message_center,
	  bus_address,
	  address,
	  data
	}).

-record(digiplex_read_command,
	{
	  offset,  %% 0 = 16 bytes, 5 bits 
	  bus_address = 0,  %% 7 bits
	  address  %% 16#8000 + addr = RAM, 16#0000 +addr = EEPROM
	}).

-record(digiplex_time,
	{
	  address=0,
	  century,
	  year,
	  month,
	  day
	}).

-define(DIGIPLEX_NOP,          16#0).
-define(DIGIPLEX_FULL_ARM,     16#2).
-define(DIGIPLEX_STAY_ARM,     16#3).
-define(DIGIPLEX_INSTANT_ARM,  16#4).
-define(DIGIPLEX_FORCE_ARM,    16#5).
-define(DIGIPLEX_DISARM,       16#6).
-define(DIGIPLEX_BEEP,         16#8).

-record(digiplex_monitor,
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

-record(digiplex_event,
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
