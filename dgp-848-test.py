import serial
from binascii import hexlify

MESSAGE_LENGTH = 37

with serial.Serial('/dev/ttyUSB0', 19200, timeout=0.1) as ser:

    def add_checksum_and_send(data):
        data = checksum_calculate(data)
        init_string = hexlify(data, ' ')
        print(f'Sending \n{init_string}')
        if ser.write(data) != len(data):
            print('Error writing data to UART')
            exit()
    
    def checksum_calculate(data):
        checksum = 0
        for x in range(len(data)-1):  
            checksum += data[x]
        
        print(f"calculate checksum for {checksum}")
        while (checksum > 255) :
            intch =int(checksum / 256)
            checksum = checksum - intch * 256
        print(f"checksum = {checksum}")
        #checksum = checksum #& 0xFF
        #utils.trace(type(checksum))
        data[36] =  checksum
        return data
        
    ser.flush()

    login_resp = bytearray()
    password = 1

    while len(login_resp) == 0 and password < 10000:
        init_message = bytearray(MESSAGE_LENGTH)
        init_message[0] = 0x5f
        init_message[1] = 0x20

        add_checksum_and_send(init_message)

        print('wait for an answer')
        rinitMessage = bytearray(ser.read(37))
        if(len(rinitMessage) != 37):
            print(f'not enough data received: {len(rinitMessage)} instead of 37')
            break
        print(hexlify(rinitMessage, ' '))

        data = rinitMessage
        data[8] = (int(password / 1000) << 4) + int(password / 100) % 10
        data[9] = ((int(password / 10) % 10) << 4) + (password % 10)

        print(f'password: {password}, data: {data[8]}, {data[9]}')

        add_checksum_and_send(data)
        login_resp = bytearray(ser.read(37))
        password += 1
    
    print(hexlify(login_resp, ' '))