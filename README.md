# digiplex


uart:send

init_string() ->
    <<16#5F, 16#20, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 
      16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 
      16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 
      16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 16#00, 
      16#00, 16#00, 16#00, 16#00, 16#7F>>.

send_pdu(<request type>, <state>)

encode_pdu(#digiplex_read_req { count=Count,
				bus_address=BusAddress,
				address=Address }) 
  when Count >= 1, Count =< 32 ->
    <<?DIGIPLEX_READ:4,
      Count:5, BusAddress:7,  %% Count=32 => Count=0
      Address:16>>;

get_info : 
    count = 12
    bud_address = 0
    address = 0x153

syntax :
    << Count: 5 >> -> 5 bits segment of Count

init:
  <<?DIGIPLEX_INIT:4,0:4, AA, EEADDR:16, II, SS, RR, CC, WW:16, MM:16, ID, E2, EV, FW:16, NN:32, DD:24>>;

  [ 0, address, eeaddr_msb, eeard_lsb, product_id, soft_ver, soft_rev, soft_id, password_msb, password_lsb, module_id_msb, module_id_lsb, ]

  0  <<?DIGIPLEX_INIT:4,0:4, # 0
  1  AA,                     # address
  2  EEADDR:16,              # eaddre_msb
  3                          # eaddre_lsb
  4  II,                     # product_id
  5  SS,                     # software version
  6  RR,                     # software revision
  7  CC,                     # software id
  8  WW:16,                  # password msb
  9                          # password lsb
  10  MM:16,                  # module_id_msb
  11                          # module_id_lsb
  12  ID,                     # winload type id
  13  E2,                     # memory map version
  14  EV,                     # event list version
  15  FW:16,                  # firmware build version msb
  16                          # firmware build version lsb
  17  NN:32,                  # module serial number bit4
  18                          # module serial number bit3
  19                          # module serial number bit2
  20                          # module serial number bit1
  21  DD:24>>;                # section data bit 3
  22                          # section data bit 2
  23                          # section data bit 1

0  00 
1  00 
2  08 # eaddr msb
3  10 # eaddr lsb
4  00 # product ID
5  04 # version 
6  14 # révision
7  00 # software ID
8  48 # password MSB
9  17 # password LSB
10 00 # module ID MSB
11 00 # module ID LSB
12 06 # winload type id
13 12 # memory map revision
14 02 # event list revision
15 a0 # firmware build revision msb
16 96 # firmware build revision lsb
17 00 
18 00 
