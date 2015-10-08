# control program for phytoflash mediated seapoint flurometer calibration

import serial
import time

#phytoflashCom = raw_input('enter phytoflash com port e.g. 'com6' > ')
#seapointCom = raw_input('enter seapoint com port e.g. 'com4' > ')
phytoflashCom = 'com1'
seapointCom = 'com4'
phytoflash = serial.Serial(phytoflashCom, 57600, timeout=1) # defaults to 8/N/1 connection
seapoint = serial.Serial(seapointCom, 9600, timeout=2, xonxoff=1) # defaults to 8/N/1 connection
print 'opening port on %s for phytoflash...' % phytoflash.portstr
print 'opening port on %s for seapoint...' % seapoint.portstr

def phyto():
    PHYTOMODE = 'NULL'
    def connect():
        global PHYTOMODE
        raw_input('plug in phytoflash and press enter...')
        print 'connecting to phytoflash...'
        time.sleep(10)
        print phytoflash.readline(),
        time.sleep(20)
        if 'Select User Mode:\r\n' in phytoflash.readlines():
            PHYTOMODE = 'root'
            print "phytoflash set to %s mode" % PHYTOMODE
    def setmode():
        global PHYTOMODE
        if PHYTOMODE == 'root':
            phytoflash.write('4\n')
            time.sleep(1)
            if '<R> Run Sample' in phytoflash.readlines():
                PHYTOMODE = 'lab'
                print "phytoflash set to %s mode" % PHYTOMODE
        else:
            phytoflash.write('\003\n')
            PHYTOMODE = 'root'
            print "phytoflash set to %s mode" % PHYTOMODE
            phytoflash.write('R\n')
            PHYTOMODE = 'lab'
            print "phytoflash set to %s mode" % PHYTOMODE
    def sample():
        phytoflash.write('R\n')
        print 'Doing sample'
        data = []
        time.sleep(8)
        for line in phytoflash:
            print line,
            data.append(line.rstrip('\r\n'))
        print data
    setmode()
    sample()


def collect_seapoint_burst():
    data = []
    data.append(['Range','FTU','DateTime'])
    for i in range(0,20):
        dataline = seapoint.readline().rstrip('\r\n')
        dataline = dataline.split(',')
        dataline.append(time.strftime('%Y-%m-%d %H:%M:%S',time.gmtime()))
        data.append(dataline)
    return(data)

#for line in collect_seapoint_burst():
    #print line

phyto.connect()
print phyto.PHYTOMODE
phyto.sample()
phytoflash.close()
seapoint.close()
