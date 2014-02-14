v 20121203 2
C 48400 25400 1 0 0 bbb-p8.sym
{
T 50800 35200 5 10 1 1 0 6 1
refdes=P8
T 48800 35400 5 10 0 0 0 0 1
device=BeagleBone
T 48800 35600 5 10 0 0 0 0 1
footprint=CONNECTOR 46 2
}
C 74400 30400 1 0 0 bbb-p9.sym
{
T 76800 40200 5 10 1 1 0 6 1
refdes=P9
T 74800 40400 5 10 0 0 0 0 1
device=BeagleBone
T 74800 40600 5 10 0 0 0 0 1
footprint=CONNECTOR 46 2
}
C 45100 18700 0 0 0 title-D.sym
C 58000 28600 1 0 0 ad7411.sym
{
T 60400 32700 5 10 1 1 0 6 1
refdes=U3
T 59200 29600 5 10 0 0 0 0 1
device=ADT7411
T 59200 29800 5 10 0 0 0 0 1
footprint=QSOP-16
}
C 62300 18100 1 0 0 lcd.sym
{
T 69900 21000 5 10 1 1 0 6 1
refdes=LCD
T 63500 20700 5 10 0 0 0 0 1
device=LCD
T 63500 20900 5 10 0 0 0 0 1
footprint=JUMPER16
}
C 53100 19500 1 0 0 xbee.sym
{
T 56500 24100 5 10 1 1 0 6 1
refdes=XBEE
T 54300 20150 5 10 0 0 0 0 1
device=XBee
T 54300 20350 5 10 0 0 0 0 1
footprint=xbee
}
C 67800 36000 1 0 0 relay.sym
{
T 68500 38350 5 10 0 1 0 0 1
device=RELAY
T 67400 38350 5 10 1 1 0 0 1
refdes=RELAY
T 67800 36000 5 10 0 1 0 0 1
footprint=relay
}
C 74900 21600 1 0 0 encoder.sym
{
T 75300 24200 5 10 0 0 0 0 1
footprint=encoder-with-switch
T 77250 24150 5 10 1 1 0 6 1
refdes=ENC
T 76100 22250 5 10 0 0 0 0 1
device=Rotary encoder
}
C 48200 34800 1 270 0 gnd-1.sym
C 77400 39600 1 90 0 gnd-1.sym
C 74200 39800 1 270 0 gnd-1.sym
C 74500 39100 1 90 0 3.3V-plus-1.sym
C 77100 39500 1 270 0 3.3V-plus-1.sym
C 74500 38300 1 90 0 5V-plus-1.sym
C 77100 38700 1 270 0 5V-plus-1.sym
C 77400 31200 1 90 0 gnd-1.sym
C 77400 30800 1 90 0 gnd-1.sym
C 74200 31000 1 270 0 gnd-1.sym
C 74200 31400 1 270 0 gnd-1.sym
C 52900 20100 1 270 0 gnd-1.sym
N 53200 23200 52700 23200 4
{
T 52700 23200 5 10 1 1 0 6 1
netname=XBEE_RXD
}
N 53200 22800 52700 22800 4
{
T 52700 22800 5 10 1 1 0 6 1
netname=XBEE_TXD
}
C 48100 36700 1 0 0 at24c01.sym
{
T 49900 39200 5 10 1 1 0 6 1
refdes=U1
T 49000 38300 5 10 0 0 0 0 1
device=AT24C01
T 49000 38500 5 10 0 0 0 0 1
footprint=TSSOP-65P-640L1-8N
}
N 48200 38700 47500 38700 4
{
T 47400 38700 5 10 1 1 0 6 1
netname=I2C2_SCL
}
N 48200 38300 47500 38300 4
{
T 47400 38300 5 10 1 1 0 6 1
netname=I2C2_SDA
}
N 74500 36100 73800 36100 4
{
T 73700 36100 5 10 1 1 0 6 1
netname=I2C2_SCL
}
N 77100 36100 77800 36100 4
{
T 77900 36100 5 10 1 1 0 0 1
netname=I2C2_SDA
}
N 51100 27500 51800 27500 4
{
T 51900 27500 5 10 1 1 0 0 1
netname=XBEE_RXD
}
N 48500 27500 47800 27500 4
{
T 47700 27500 5 10 1 1 0 6 1
netname=XBEE_TXD
}
N 51100 32700 51800 32700 4
{
T 51900 32700 5 10 1 1 0 0 1
netname=HEATER_EN
}
N 65500 35500 65000 35500 4
{
T 64900 35600 5 10 1 1 180 0 1
netname=HEATER_EN
}
C 67400 36900 1 90 0 diode-1.sym
{
T 66800 37300 5 10 0 0 90 0 1
device=DIODE
T 66900 37200 5 10 1 1 90 0 1
refdes=D1
T 67400 36900 5 10 0 0 0 0 1
footprint=1206
}
N 67200 38800 67200 37800 4
N 67200 36900 67200 36000 4
N 67200 36000 68100 36000 4
C 67100 34700 1 0 0 gnd-1.sym
N 65200 21200 65200 21700 4
{
T 65200 21800 5 10 1 1 90 0 1
netname=LCD_D0
}
N 65600 21200 65600 21700 4
{
T 65600 21800 5 10 1 1 90 0 1
netname=LCD_D1
}
N 66000 21200 66000 21700 4
{
T 66000 21800 5 10 1 1 90 0 1
netname=LCD_D2
}
N 66400 21200 66400 21700 4
{
T 66400 21800 5 10 1 1 90 0 1
netname=LCD_D3
}
N 64800 21200 64800 21700 4
{
T 64800 21800 5 10 1 1 90 0 1
netname=LCD_E
}
N 64000 21200 64000 21700 4
{
T 64000 21800 5 10 1 1 90 0 1
netname=LCD_RS
}
C 59300 28400 1 0 0 gnd-1.sym
C 51400 34600 1 90 0 gnd-1.sym
C 49100 36500 1 0 0 gnd-1.sym
C 49000 39400 1 0 0 3.3V-plus-1.sym
C 48000 38700 1 90 0 resistor-1.sym
{
T 47600 39000 5 10 0 0 90 0 1
device=RESISTOR
T 47700 38900 5 10 1 1 90 0 1
refdes=R1
T 47700 39200 5 10 1 1 90 0 1
value=10k
T 48000 38700 5 10 0 1 0 0 1
footprint=0603
}
C 48000 37400 1 90 0 resistor-1.sym
{
T 47600 37700 5 10 0 0 90 0 1
device=RESISTOR
T 47700 37600 5 10 1 1 90 0 1
refdes=R2
T 47700 37900 5 10 1 1 90 0 1
value=10k
T 48000 37400 5 10 0 1 0 0 1
footprint=0603
}
C 47700 39600 1 0 0 3.3V-plus-1.sym
C 48100 37400 1 180 0 3.3V-plus-1.sym
C 50200 38100 1 270 0 3.3V-plus-1.sym
C 50500 37400 1 90 0 gnd-1.sym
C 50200 38500 1 270 0 3.3V-plus-1.sym
N 73800 22900 74100 22900 4
{
T 73700 22900 5 10 1 1 0 6 1
netname=ENC_B
}
N 78100 23300 77600 23300 4
{
T 78150 23250 5 10 1 1 0 0 1
netname=ENC_SW
}
N 73800 23700 74100 23700 4
{
T 73700 23700 5 10 1 1 0 6 1
netname=ENC_A
}
C 59200 32900 1 0 0 3.3V-plus-1.sym
C 63000 21200 1 0 0 3.3V-plus-1.sym
C 62900 21500 1 180 0 gnd-1.sym
N 60700 31400 61300 31400 4
{
T 61400 31400 5 10 1 1 0 0 1
netname=I2C2_SDA
}
N 60700 32200 61300 32200 4
{
T 61400 32200 5 10 1 1 0 0 1
netname=ADC_INT
}
N 60700 31000 61300 31000 4
{
T 61400 31000 5 10 1 1 0 0 1
netname=I2C2_SCL
}
N 63900 27200 63200 27200 4
{
T 63100 27200 5 10 1 1 0 6 1
netname=I2C2_SDA
}
N 63900 27600 63200 27600 4
{
T 63100 27600 5 10 1 1 0 6 1
netname=I2C2_SCL
}
C 65000 31500 1 0 0 3.3V-plus-1.sym
C 65100 23800 1 0 0 gnd-1.sym
N 53200 22000 52700 22000 4
{
T 52600 22000 5 10 1 1 0 6 1
netname=XBEE_\_RESET\_
}
N 48500 27100 47800 27100 4
{
T 47700 27100 5 10 1 1 0 6 1
netname=XBEE_\_RESET\_
}
N 67200 38800 68100 38800 4
N 66500 26000 66700 26000 4
C 66700 25900 1 0 0 resistor-1.sym
{
T 67000 26300 5 10 0 0 0 0 1
device=RESISTOR
T 66900 26100 5 10 1 1 0 0 1
refdes=R22
T 67300 26100 5 10 0 1 0 0 1
value=1k
T 66700 25900 5 10 0 1 0 0 1
footprint=0603
T 67400 26100 5 10 1 1 0 0 1
value=180
}
C 66700 25500 1 0 0 resistor-1.sym
{
T 67000 25900 5 10 0 0 0 0 1
device=RESISTOR
T 66900 25700 5 10 1 1 0 0 1
refdes=R23
T 67300 25700 5 10 0 1 0 0 1
value=1k
T 66700 25500 5 10 0 1 0 0 1
footprint=0603
T 67400 25700 5 10 1 1 0 0 1
value=180
}
C 66700 25100 1 0 0 resistor-1.sym
{
T 67000 25500 5 10 0 0 0 0 1
device=RESISTOR
T 66900 25300 5 10 1 1 0 0 1
refdes=R24
T 67300 25300 5 10 0 1 0 0 1
value=1k
T 66700 25100 5 10 0 1 0 0 1
footprint=0603
T 67400 25300 5 10 1 1 0 0 1
value=180
}
N 66500 25600 66700 25600 4
N 66500 25200 66700 25200 4
N 66500 24800 66700 24800 4
N 67600 24800 67900 24800 4
{
T 68000 24700 5 10 1 1 0 0 1
netname=LCD_BACKLT
}
N 53200 20400 52700 20400 4
{
T 52600 20400 5 10 1 1 0 6 1
netname=XBEE_SLEEP_RQ
}
N 48500 28700 48000 28700 4
{
T 47900 28700 5 10 1 1 0 6 1
netname=XBEE_\_CTS\_
}
N 51100 28700 51800 28700 4
{
T 51900 28700 5 10 1 1 0 0 1
netname=XBEE_\_RTS\_
}
N 51100 27100 51800 27100 4
{
T 51900 27100 5 10 1 1 0 0 1
netname=XBEE_SLEEP_RQ
}
C 57000 29100 1 0 0 resistor-1.sym
{
T 57300 29500 5 10 0 0 0 0 1
device=RESISTOR
T 57200 29300 5 10 1 1 0 0 1
refdes=R17
T 57000 29100 5 10 0 1 0 0 1
footprint=0603
}
C 57000 29000 1 90 0 3.3V-plus-1.sym
N 57900 29200 58000 29200 4
C 65500 35400 1 0 0 resistor-1.sym
{
T 65800 35800 5 10 0 0 0 0 1
device=RESISTOR
T 65700 35700 5 10 1 1 0 0 1
refdes=R21
T 66200 35700 5 10 1 1 0 0 1
value=1k
T 65500 35400 5 10 0 1 0 0 1
footprint=0603
}
C 68900 21500 1 180 0 gnd-1.sym
N 68400 21200 68400 21700 4
{
T 68400 21800 5 10 1 1 90 0 1
netname=LCD_BACKLT
}
N 66800 21200 66800 21700 4
{
T 66800 21800 5 10 1 1 90 0 1
netname=LCD_D4
}
N 67200 21200 67200 21700 4
{
T 67200 21800 5 10 1 1 90 0 1
netname=LCD_D5
}
N 67600 21200 67600 21700 4
{
T 67600 21800 5 10 1 1 90 0 1
netname=LCD_D6
}
N 68000 21200 68000 21700 4
{
T 68000 21800 5 10 1 1 90 0 1
netname=LCD_D7
}
N 67600 25600 68100 25600 4
N 67600 26000 68100 26000 4
N 51100 31900 51800 31900 4
{
T 51900 31900 5 10 1 1 0 0 1
netname=LCD_D7
}
N 47800 31900 48500 31900 4
{
T 47700 31900 5 10 1 1 0 6 1
netname=ENC_A
}
N 47800 32300 48500 32300 4
{
T 47700 32300 5 10 1 1 0 6 1
netname=ENC_SW
}
N 74500 34100 73800 34100 4
{
T 73700 34100 5 10 1 1 0 6 1
netname=SPI1_MOSI
}
N 74500 33700 73800 33700 4
{
T 73700 33700 5 10 1 1 0 6 1
netname=SPI1_SCLK
}
N 77100 34100 77800 34100 4
{
T 77900 34100 5 10 1 1 0 0 1
netname=SPI1_MISO
}
C 57000 29500 1 0 0 resistor-1.sym
{
T 57300 29900 5 10 0 0 0 0 1
device=RESISTOR
T 57200 29700 5 10 1 1 0 0 1
refdes=R16
T 57000 29500 5 10 0 1 0 0 1
footprint=0603
}
C 57000 29400 1 90 0 3.3V-plus-1.sym
N 57900 29600 58000 29600 4
C 57000 29900 1 0 0 resistor-1.sym
{
T 57300 30300 5 10 0 0 0 0 1
device=RESISTOR
T 57200 30100 5 10 1 1 0 0 1
refdes=R15
T 57000 29900 5 10 0 1 0 0 1
footprint=0603
}
C 57000 29800 1 90 0 3.3V-plus-1.sym
N 57900 30000 58000 30000 4
C 57000 30300 1 0 0 resistor-1.sym
{
T 57300 30700 5 10 0 0 0 0 1
device=RESISTOR
T 57200 30500 5 10 1 1 0 0 1
refdes=R14
T 57000 30300 5 10 0 1 0 0 1
footprint=0603
}
C 57000 30200 1 90 0 3.3V-plus-1.sym
N 57900 30400 58000 30400 4
C 57000 30700 1 0 0 resistor-1.sym
{
T 57300 31100 5 10 0 0 0 0 1
device=RESISTOR
T 57200 30900 5 10 1 1 0 0 1
refdes=R11
T 57000 30700 5 10 0 1 0 0 1
footprint=0603
}
C 57000 30600 1 90 0 3.3V-plus-1.sym
N 57900 30800 58000 30800 4
C 57000 31200 1 0 0 resistor-1.sym
{
T 57300 31600 5 10 0 0 0 0 1
device=RESISTOR
T 57100 31100 5 10 1 1 0 0 1
refdes=R10
T 57000 31200 5 10 0 1 0 0 1
footprint=0603
}
C 57000 31100 1 90 0 3.3V-plus-1.sym
N 57900 31300 58100 31300 4
C 47200 21600 1 0 0 lp3982.sym
{
T 49000 24100 5 10 1 1 0 6 1
refdes=U2
T 47600 24300 5 10 0 0 0 0 1
device=LP3982
T 47600 24500 5 10 0 0 0 0 1
footprint=SON-8
}
C 47300 23400 1 90 0 5V-plus-1.sym
C 46400 23000 1 0 0 capacitor-1.sym
{
T 46600 23700 5 10 0 0 0 0 1
device=CAPACITOR
T 46500 23300 5 10 1 1 0 0 1
refdes=C6
T 46600 23900 5 10 0 0 0 0 1
symversion=0.1
T 46400 23000 5 10 1 1 0 0 1
value=33n
T 46400 23000 5 10 0 1 0 0 1
footprint=0603
}
C 46100 23300 1 270 0 gnd-1.sym
C 48200 21400 1 0 0 gnd-1.sym
N 47300 22800 46700 22800 4
{
T 46600 22800 5 10 1 1 0 6 1
netname=XBEE_PWR
}
N 49300 23200 49300 23600 4
N 49300 23600 53200 23600 4
N 51100 26700 51800 26700 4
{
T 51900 26700 5 10 1 1 0 0 1
netname=XBEE_PWR
}
N 56800 23200 57300 23200 4
{
T 57400 23200 5 10 1 1 180 6 1
netname=XBEE_\_CTS\_
}
N 56800 21600 57300 21600 4
{
T 57400 21600 5 10 1 1 180 6 1
netname=XBEE_\_RTS\_
}
C 63700 22100 1 90 0 pot-1.sym
{
T 62800 22900 5 10 0 0 90 0 1
device=VARIABLE_RESISTOR
T 63300 22900 5 10 1 1 180 0 1
refdes=V0
T 62200 22900 5 10 0 0 90 0 1
footprint=trimmer-resistor
T 63300 22400 5 10 1 1 180 0 1
value=10k
}
N 63600 21200 63600 22100 4
C 63700 23300 1 180 0 gnd-1.sym
C 62800 22700 1 270 0 gnd-1.sym
N 66400 35500 66700 35500 4
C 64500 21500 1 180 0 gnd-1.sym
N 51100 29900 51800 29900 4
{
T 51900 29900 5 10 1 1 0 0 1
netname=LCD_D3
}
C 55900 40200 1 0 0 3.3V-plus-1.sym
C 56200 38200 1 90 0 resistor-1.sym
{
T 55800 38500 5 10 0 0 90 0 1
device=RESISTOR
T 56000 38400 5 10 1 1 90 0 1
refdes=R6
T 56000 38700 5 10 1 1 90 0 1
value=1k
T 56200 38200 5 10 0 1 90 0 1
footprint=0603
}
C 57400 40200 1 0 0 3.3V-plus-1.sym
C 57900 38200 1 90 0 resistor-1.sym
{
T 57500 38500 5 10 0 0 90 0 1
device=RESISTOR
T 57700 38400 5 10 1 1 90 0 1
refdes=R7
T 57700 38700 5 10 1 1 90 0 1
value=1k
T 57900 38200 5 10 0 1 90 0 1
footprint=0603
}
N 56100 39100 55500 39100 4
{
T 55400 39100 5 10 1 1 0 6 1
netname=BTN1
}
N 57800 39100 57200 39100 4
{
T 57100 39100 5 10 1 1 0 6 1
netname=BTN2
}
N 48500 33500 47800 33500 4
{
T 47700 33500 5 10 1 1 0 6 1
netname=BTN4
}
N 48500 33100 47800 33100 4
{
T 47700 33100 5 10 1 1 0 6 1
netname=BTN2
}
C 70300 39400 1 90 0 connector2-2.sym
{
T 69000 40100 5 10 1 1 90 6 1
refdes=HEAT
T 69050 39700 5 10 0 0 90 0 1
device=CONNECTOR_2
T 68850 39700 5 10 0 0 90 0 1
footprint=JUMPER2
}
N 69900 39400 69900 38800 4
N 69500 39400 69500 38850 4
N 69500 38850 69400 38850 4
C 60400 34800 1 90 0 resistor-1.sym
{
T 60000 35100 5 10 0 0 90 0 1
device=RESISTOR
T 60200 34800 5 10 1 1 90 0 1
refdes=R18
T 60200 35300 5 10 1 1 90 0 1
value=1k
T 60400 34800 5 10 0 1 0 0 1
footprint=0603
}
N 60300 34800 60300 34200 4
{
T 60300 34100 5 10 1 1 90 6 1
netname=SPI1_MISO
}
C 60100 35700 1 0 0 3.3V-plus-1.sym
C 61000 34800 1 90 0 resistor-1.sym
{
T 60600 35100 5 10 0 0 90 0 1
device=RESISTOR
T 60800 34800 5 10 1 1 90 0 1
refdes=R19
T 60800 35300 5 10 1 1 90 0 1
value=1k
T 61000 34800 5 10 0 1 0 0 1
footprint=0603
}
N 60900 34800 60900 34200 4
{
T 60900 34100 5 10 1 1 90 6 1
netname=SPI1_MOSI
}
C 60700 35700 1 0 0 3.3V-plus-1.sym
C 61600 34800 1 90 0 resistor-1.sym
{
T 61200 35100 5 10 0 0 90 0 1
device=RESISTOR
T 61400 34800 5 10 1 1 90 0 1
refdes=R20
T 61400 35300 5 10 1 1 90 0 1
value=1k
T 61600 34800 5 10 0 1 0 0 1
footprint=0603
}
C 61300 35700 1 0 0 3.3V-plus-1.sym
N 61500 34800 61500 34200 4
{
T 61500 34100 5 10 1 1 90 6 1
netname=SPI1_SCLK
}
C 46700 24000 1 90 0 capacitor-1.sym
{
T 46000 24200 5 10 0 0 90 0 1
device=CAPACITOR
T 46400 24100 5 10 1 1 90 0 1
refdes=C3
T 45800 24200 5 10 0 0 90 0 1
symversion=0.1
T 46700 24000 5 10 1 1 90 0 1
value=10u
T 46700 24000 5 10 0 1 0 0 1
footprint=0603
}
C 46300 24900 1 0 0 5V-plus-1.sym
C 46400 23700 1 0 0 gnd-1.sym
C 50100 22700 1 90 0 capacitor-1.sym
{
T 49400 22900 5 10 0 0 90 0 1
device=CAPACITOR
T 49200 22900 5 10 0 0 90 0 1
symversion=0.1
T 49800 22800 5 10 1 1 90 0 1
refdes=C7
T 50100 22700 5 10 1 1 90 0 1
value=2.2u
T 50100 22700 5 10 0 1 0 0 1
footprint=0603
}
C 49800 22400 1 0 0 gnd-1.sym
C 59100 33700 1 90 0 capacitor-1.sym
{
T 58400 33900 5 10 0 0 90 0 1
device=CAPACITOR
T 58200 33900 5 10 0 0 90 0 1
symversion=0.1
T 58800 33800 5 10 1 1 90 0 1
refdes=C5
T 59100 33700 5 10 1 1 90 0 1
value=100n
T 59100 33700 5 10 0 1 0 0 1
footprint=0603
}
C 58700 34600 1 0 0 3.3V-plus-1.sym
C 58800 33400 1 0 0 gnd-1.sym
C 58500 33700 1 90 0 capacitor-1.sym
{
T 57800 33900 5 10 0 0 90 0 1
device=CAPACITOR
T 57600 33900 5 10 0 0 90 0 1
symversion=0.1
T 58200 33800 5 10 1 1 90 0 1
refdes=C4
T 58500 33700 5 10 1 1 90 0 1
value=1u
T 58500 33700 5 10 0 1 0 0 1
footprint=0603
}
C 58100 34600 1 0 0 3.3V-plus-1.sym
C 58200 33400 1 0 0 gnd-1.sym
C 46100 24000 1 90 0 capacitor-1.sym
{
T 45400 24200 5 10 0 0 90 0 1
device=CAPACITOR
T 45800 24100 5 10 1 1 90 0 1
refdes=C2
T 45200 24200 5 10 0 0 90 0 1
symversion=0.1
T 46100 24000 5 10 1 1 90 0 1
value=100n
T 46100 24000 5 10 0 1 0 0 1
footprint=0603
}
C 45800 23700 1 0 0 gnd-1.sym
C 45700 24900 1 0 0 5V-plus-1.sym
C 66900 31500 1 90 0 capacitor-1.sym
{
T 66200 31700 5 10 0 0 90 0 1
device=CAPACITOR
T 66000 31700 5 10 0 0 90 0 1
symversion=0.1
T 66600 31600 5 10 1 1 90 0 1
refdes=C8
T 66900 31500 5 10 1 1 90 0 1
value=1u
T 66900 31500 5 10 0 1 0 0 1
footprint=0603
}
C 66500 32400 1 0 0 3.3V-plus-1.sym
C 66600 31200 1 0 0 gnd-1.sym
C 45900 39000 1 90 0 capacitor-1.sym
{
T 45200 39200 5 10 0 0 90 0 1
device=CAPACITOR
T 45000 39200 5 10 0 0 90 0 1
symversion=0.1
T 45600 39100 5 10 1 1 90 0 1
refdes=C1
T 45900 39000 5 10 1 1 90 0 1
value=1u
T 45900 39000 5 10 0 1 0 0 1
footprint=0603
}
C 45500 39900 1 0 0 3.3V-plus-1.sym
C 45600 38700 1 0 0 gnd-1.sym
N 74500 35300 73800 35300 4
{
T 73700 35300 5 10 1 1 0 6 1
netname=ADC_INT
}
C 51000 23600 1 90 1 led-1.sym
{
T 50400 22800 5 10 0 0 90 6 1
device=LED
T 51200 23500 5 10 1 1 90 6 1
refdes=LED_XBEE
T 50200 22800 5 10 0 0 90 6 1
symversion=0.1
T 51000 23600 5 10 0 0 0 0 1
footprint=0603
}
C 50900 21800 1 90 0 resistor-1.sym
{
T 50500 22100 5 10 0 0 90 0 1
device=RESISTOR
T 50700 21800 5 10 1 1 90 0 1
refdes=R29
T 50700 22300 5 10 1 1 90 0 1
value=220
T 50900 21800 5 10 0 1 0 0 1
footprint=0603
}
C 50700 21500 1 0 0 gnd-1.sym
C 66700 35000 1 0 0 bc817.sym
{
T 67300 35500 5 10 0 0 0 0 1
device=NPN_TRANSISTOR
T 66800 35900 5 10 1 1 0 0 1
refdes=Q1
T 67300 36300 5 10 0 1 0 0 1
footprint=SOT23
T 67100 35500 5 10 1 1 0 0 1
model=BC817
}
N 56500 31000 58100 31000 4
{
T 56000 31000 5 10 1 1 0 0 1
netname=EXT3
}
N 58000 31000 58000 30800 4
N 56500 30600 58100 30600 4
{
T 56000 30600 5 10 1 1 0 0 1
netname=EXT5
}
N 58000 30600 58000 30400 4
N 56500 30200 58100 30200 4
{
T 56000 30200 5 10 1 1 0 0 1
netname=EXT4
}
N 58000 30200 58000 30000 4
N 56500 29800 58100 29800 4
{
T 56000 29800 5 10 1 1 0 0 1
netname=EXT2
}
N 58000 29800 58000 29600 4
N 56500 29400 58100 29400 4
{
T 56000 29400 5 10 1 1 0 0 1
netname=EXT1
}
N 58000 29400 58000 29200 4
C 56700 31700 1 270 0 gnd-1.sym
C 53300 31100 1 270 0 gnd-1.sym
C 53300 30700 1 270 0 gnd-1.sym
C 53300 30300 1 270 0 gnd-1.sym
C 53300 29900 1 270 0 gnd-1.sym
C 53300 29500 1 270 0 gnd-1.sym
C 67500 38800 1 0 0 5V-plus-1.sym
C 75100 21600 1 90 0 resistor-1.sym
{
T 74700 21900 5 10 0 0 90 0 1
device=RESISTOR
T 75100 21600 5 10 0 1 0 0 1
footprint=0603
T 74900 21600 5 10 1 1 90 0 1
refdes=R30
T 74900 22100 5 10 1 1 90 0 1
value=1k
}
N 70600 22200 70600 21800 4
{
T 70300 22400 5 10 1 1 180 6 1
netname=ENC_B
}
C 75200 21600 1 180 0 3.3V-plus-1.sym
N 71500 22200 71500 21800 4
{
T 71200 22400 5 10 1 1 180 6 1
netname=ENC_A
}
C 72600 22200 1 90 0 resistor-1.sym
{
T 72200 22500 5 10 0 0 90 0 1
device=RESISTOR
T 72600 22200 5 10 0 1 0 0 1
footprint=0603
T 72400 22200 5 10 1 1 90 0 1
refdes=R32
T 72400 22700 5 10 1 1 90 0 1
value=1k
}
N 72500 22200 72500 21800 4
{
T 71700 22000 5 10 1 1 180 6 1
netname=ENC_SW
}
C 72300 23100 1 0 0 3.3V-plus-1.sym
N 51100 33100 51800 33100 4
{
T 51900 33100 5 10 1 1 0 0 1
netname=BTN1
}
C 56500 39200 1 90 0 smt-pushbutton-no.sym
{
T 55900 39600 5 10 1 1 90 0 1
refdes=BTN1
T 55900 39600 5 10 0 0 90 0 1
device=SWITCH_PUSHBUTTON_NO
T 56500 39200 5 10 0 1 90 0 1
footprint=tl3301
}
C 58000 39200 1 90 0 smt-pushbutton-no.sym
{
T 57400 39600 5 10 1 1 90 0 1
refdes=BTN2
T 57400 39600 5 10 0 0 90 0 1
device=SWITCH_PUSHBUTTON_NO
T 58000 39200 5 10 0 1 90 0 1
footprint=tl3301
}
C 57700 37900 1 0 0 gnd-1.sym
C 56000 37900 1 0 0 gnd-1.sym
C 59500 40200 1 0 0 3.3V-plus-1.sym
C 59600 38200 1 90 0 resistor-1.sym
{
T 59200 38500 5 10 0 0 90 0 1
device=RESISTOR
T 59600 38200 5 10 0 1 90 0 1
footprint=0603
T 59400 38400 5 10 1 1 90 0 1
refdes=R8
T 59400 38700 5 10 1 1 90 0 1
value=1k
}
N 59500 39100 58900 39100 4
{
T 58800 39100 5 10 1 1 0 6 1
netname=BTN3
}
C 59900 39200 1 90 0 smt-pushbutton-no.sym
{
T 59300 39600 5 10 0 0 90 0 1
device=SWITCH_PUSHBUTTON_NO
T 59900 39200 5 10 0 1 90 0 1
footprint=tl3301
T 59300 39600 5 10 1 1 90 0 1
refdes=BTN3
}
C 59400 37900 1 0 0 gnd-1.sym
C 61200 40200 1 0 0 3.3V-plus-1.sym
C 61300 38200 1 90 0 resistor-1.sym
{
T 60900 38500 5 10 0 0 90 0 1
device=RESISTOR
T 61300 38200 5 10 0 1 90 0 1
footprint=0603
T 61100 38400 5 10 1 1 90 0 1
refdes=R9
T 61100 38700 5 10 1 1 90 0 1
value=1k
}
N 61200 39100 60600 39100 4
{
T 60500 39100 5 10 1 1 0 6 1
netname=BTN4
}
C 61600 39200 1 90 0 smt-pushbutton-no.sym
{
T 61000 39600 5 10 0 0 90 0 1
device=SWITCH_PUSHBUTTON_NO
T 61600 39200 5 10 0 1 90 0 1
footprint=tl3301
T 61000 39600 5 10 1 1 90 0 1
refdes=BTN4
}
C 61100 37900 1 0 0 gnd-1.sym
N 57800 39200 57800 39100 4
N 56100 39200 56100 39100 4
N 59500 39200 59500 39100 4
N 61200 39200 61200 39100 4
N 51100 33500 51800 33500 4
{
T 51900 33500 5 10 1 1 0 0 1
netname=BTN3
}
N 69400 38800 69400 38850 4
N 67600 25200 68100 25200 4
C 69500 26400 1 180 0 rgb-led.sym
{
T 68700 25800 5 10 0 0 180 0 1
device=LED
T 69050 26300 5 10 1 1 180 0 1
refdes=RGB_LED
T 68700 25600 5 10 0 0 180 0 1
symversion=0.1
T 69500 26400 5 10 0 0 180 6 1
footprint=PLCC4
}
N 68100 26000 68100 25900 4
N 68100 25200 68100 25300 4
C 69200 25800 1 270 0 3.3V-plus-1.sym
C 74600 28900 1 180 0 led-1.sym
{
T 73800 28300 5 10 0 0 180 0 1
device=LED
T 74800 28900 5 10 1 1 180 0 1
refdes=LED4
T 73800 28100 5 10 0 0 180 0 1
symversion=0.1
T 74600 28900 5 10 0 1 0 0 1
footprint=0603
}
N 72500 29000 72300 29000 4
C 73400 28600 1 180 0 led-1.sym
{
T 72600 28000 5 10 0 0 180 0 1
device=LED
T 73600 28600 5 10 1 1 180 0 1
refdes=LED7
T 72600 27800 5 10 0 0 180 0 1
symversion=0.1
T 73400 28600 5 10 0 1 0 0 1
footprint=0603
}
N 72300 28700 73700 28700 4
C 74600 28300 1 180 0 led-1.sym
{
T 73800 27700 5 10 0 0 180 0 1
device=LED
T 74800 28300 5 10 1 1 180 0 1
refdes=LED8
T 73800 27500 5 10 0 0 180 0 1
symversion=0.1
T 74600 28300 5 10 0 1 0 0 1
footprint=0603
}
C 74600 28900 1 270 0 3.3V-plus-1.sym
C 73400 28600 1 270 0 3.3V-plus-1.sym
C 74600 28300 1 270 0 3.3V-plus-1.sym
T 72100 19400 9 12 1 0 0 0 1
Heater control cape for BeagleBone
N 47800 29500 48500 29500 4
{
T 47700 29500 5 10 1 1 0 6 1
netname=LCD_D1
}
N 47800 29100 48500 29100 4
{
T 47700 29100 5 10 1 1 0 6 1
netname=LCD_E
}
N 51100 29500 51800 29500 4
{
T 51900 29500 5 10 1 1 0 0 1
netname=LCD_D2
}
N 51100 29100 51800 29100 4
{
T 51900 29100 5 10 1 1 0 0 1
netname=LCD_D0
}
C 56800 27100 1 180 0 header8-2.sym
{
T 56800 25500 5 10 0 1 180 0 1
device=HEADER8
T 56500 27300 5 10 1 1 180 0 1
refdes=NRF24
T 56800 27100 5 10 0 0 0 0 1
footprint=CONNECTOR 4 2
}
C 55400 25500 1 90 0 3.3V-plus-1.sym
C 57100 25600 1 90 0 gnd-1.sym
N 56800 26100 57300 26100 4
{
T 57400 26100 5 10 1 1 0 0 1
netname=NRF_CE
}
N 55400 26100 54900 26100 4
{
T 54800 26100 5 10 1 1 0 6 1
netname=NRF_\_CS\_
}
N 56800 26500 57300 26500 4
{
T 57400 26500 5 10 1 1 0 0 1
netname=SPI1_SCLK
}
N 55400 26500 54900 26500 4
{
T 54800 26500 5 10 1 1 0 6 1
netname=SPI1_MOSI
}
N 55400 26900 54900 26900 4
{
T 54800 26900 5 10 1 1 0 6 1
netname=NRF_IRQ
}
N 56800 26900 57300 26900 4
{
T 57400 26900 5 10 1 1 0 0 1
netname=SPI1_MISO
}
N 74500 34500 73800 34500 4
{
T 73700 34500 5 10 1 1 0 6 1
netname=NRF_IRQ
}
N 77100 31700 77800 31700 4
{
T 77900 31800 5 10 1 1 180 6 1
netname=NRF_\_CS\_
}
N 58100 31800 56400 31800 4
N 56400 31800 56400 31900 4
N 56900 32400 56900 31800 4
N 56400 33000 58100 33000 4
N 58100 33000 58100 32200 4
N 56400 32900 56400 33000 4
C 57700 32000 1 90 0 capacitor-1.sym
{
T 57000 32200 5 10 0 0 90 0 1
device=CAPACITOR
T 56800 32200 5 10 0 0 90 0 1
symversion=0.1
T 57400 32100 5 10 1 1 90 0 1
refdes=C9
T 57700 32100 5 10 1 1 90 0 1
value=1n
T 57700 32000 5 10 0 1 0 0 1
footprint=0603
}
N 57500 32000 57500 31800 4
N 57500 32900 57500 33000 4
C 53600 29200 1 0 0 header10-2.sym
{
T 53600 31200 5 10 0 1 0 0 1
device=HEADER10
T 53900 31300 5 10 1 1 0 0 1
refdes=EXT
T 53600 29200 5 10 0 0 0 0 1
footprint=CONNECTOR 5 2
}
N 58100 31300 58100 31600 4
C 57000 31500 1 0 0 resistor-1.sym
{
T 57300 31900 5 10 0 0 0 0 1
device=RESISTOR
T 57100 31400 5 10 1 1 0 0 1
refdes=R12
T 57000 31500 5 10 0 1 0 0 1
footprint=0603
}
N 57900 31600 58100 31600 4
C 63800 24000 1 0 0 pca9685.sym
{
T 66200 31300 5 10 1 1 0 6 1
refdes=U4
T 65000 27800 5 10 0 0 0 0 1
device=PCA9685
T 65000 28000 5 10 0 0 0 0 1
footprint=SSOP-28
}
C 63600 30900 1 270 0 gnd-1.sym
C 63600 30500 1 270 0 gnd-1.sym
C 63600 30100 1 270 0 gnd-1.sym
C 63600 29700 1 270 0 gnd-1.sym
C 63600 29300 1 270 0 gnd-1.sym
C 63600 28900 1 270 0 gnd-1.sym
N 73700 28100 72300 28100 4
C 74600 27300 1 180 0 led-1.sym
{
T 73800 26700 5 10 0 0 180 0 1
device=LED
T 73800 26500 5 10 0 0 180 0 1
symversion=0.1
T 74600 27300 5 10 0 1 0 0 1
footprint=0603
T 74800 27300 5 10 1 1 180 0 1
refdes=LED6
}
C 73400 27000 1 180 0 led-1.sym
{
T 72600 26400 5 10 0 0 180 0 1
device=LED
T 72600 26200 5 10 0 0 180 0 1
symversion=0.1
T 73400 27000 5 10 0 1 0 0 1
footprint=0603
T 73600 27000 5 10 1 1 180 0 1
refdes=LED1
}
C 74600 27300 1 270 0 3.3V-plus-1.sym
C 73400 27000 1 270 0 3.3V-plus-1.sym
C 74600 26700 1 180 0 led-1.sym
{
T 73800 26100 5 10 0 0 180 0 1
device=LED
T 73800 25900 5 10 0 0 180 0 1
symversion=0.1
T 74600 26700 5 10 0 1 0 0 1
footprint=0603
T 74800 26700 5 10 1 1 180 0 1
refdes=LED2
}
C 74600 26700 1 270 0 3.3V-plus-1.sym
C 66700 24700 1 0 0 resistor-1.sym
{
T 67000 25100 5 10 0 0 0 0 1
device=RESISTOR
T 66900 24900 5 10 1 1 0 0 1
refdes=R40
T 66700 24700 5 10 0 1 0 0 1
footprint=0603
T 67400 24900 5 10 1 1 0 0 1
value=180
}
C 63600 28100 1 270 0 gnd-1.sym
C 73400 27600 1 180 0 led-1.sym
{
T 72600 27000 5 10 0 0 180 0 1
device=LED
T 73600 27600 5 10 1 1 180 0 1
refdes=LED5
T 72600 26800 5 10 0 0 180 0 1
symversion=0.1
T 73400 27600 5 10 0 1 0 0 1
footprint=0603
}
C 73400 27600 1 270 0 3.3V-plus-1.sym
C 70700 29200 1 270 0 resistor-array-8.sym
{
T 71950 29050 5 10 0 0 270 0 1
device=RESISTORPACK_8
T 70950 27950 5 10 1 1 0 0 1
refdes=R25
T 70700 29200 5 10 0 0 180 0 1
footprint=CTS-742C083
T 71800 27900 5 10 1 1 0 0 1
value=180
}
N 72300 28400 72500 28400 4
C 70700 27600 1 270 0 resistor-array-8.sym
{
T 71950 27450 5 10 0 0 270 0 1
device=RESISTORPACK_8
T 70950 26350 5 10 1 1 0 0 1
refdes=R26
T 70700 27600 5 10 0 0 180 0 1
footprint=CTS-742C083
T 71800 26300 5 10 1 1 0 0 1
value=180
}
N 72300 27400 72500 27400 4
N 72300 27100 73700 27100 4
N 72300 26500 73700 26500 4
N 72300 26800 72500 26800 4
C 73400 29200 1 180 0 led-1.sym
{
T 72600 28600 5 10 0 0 180 0 1
device=LED
T 73600 29200 5 10 1 1 180 0 1
refdes=LED3
T 72600 28400 5 10 0 0 180 0 1
symversion=0.1
T 73400 29200 5 10 0 1 0 0 1
footprint=0603
}
N 70700 29000 70200 29000 4
{
T 70400 29000 5 10 1 1 0 6 1
netname=LED3
}
N 70700 28700 70200 28700 4
{
T 70400 28700 5 10 1 1 0 6 1
netname=LED4
}
N 70700 28400 70200 28400 4
{
T 70400 28400 5 10 1 1 0 6 1
netname=LED7
}
N 70700 28100 70200 28100 4
{
T 70400 28100 5 10 1 1 0 6 1
netname=LED8
}
N 70700 27400 70200 27400 4
{
T 70400 27400 5 10 1 1 0 6 1
netname=LED5
}
N 70700 27100 70200 27100 4
{
T 70400 27100 5 10 1 1 0 6 1
netname=LED6
}
N 70700 26800 70200 26800 4
{
T 70400 26800 5 10 1 1 0 6 1
netname=LED1
}
N 70700 26500 70200 26500 4
{
T 70400 26500 5 10 1 1 0 6 1
netname=LED2
}
C 73400 29200 1 270 0 3.3V-plus-1.sym
N 66500 26800 67100 26800 4
{
T 67200 26700 5 10 1 1 0 0 1
netname=LED1
}
N 66500 26400 67100 26400 4
{
T 67200 26300 5 10 1 1 0 0 1
netname=LED2
}
N 66500 29200 67100 29200 4
{
T 67200 29100 5 10 1 1 0 0 1
netname=LED3
}
N 66500 28800 67100 28800 4
{
T 67200 28700 5 10 1 1 0 0 1
netname=LED4
}
N 66500 27600 67100 27600 4
{
T 67200 27500 5 10 1 1 0 0 1
netname=LED5
}
N 66500 27200 67100 27200 4
{
T 67200 27100 5 10 1 1 0 0 1
netname=LED6
}
N 66500 28400 67100 28400 4
{
T 67200 28300 5 10 1 1 0 0 1
netname=LED7
}
N 66500 28000 67100 28000 4
{
T 67200 27900 5 10 1 1 0 0 1
netname=LED8
}
N 55000 31000 55400 31000 4
{
T 55400 31000 5 10 1 1 0 0 1
netname=EXT1
}
N 55000 30600 55400 30600 4
{
T 55400 30600 5 10 1 1 0 0 1
netname=EXT2
}
N 55000 30200 55400 30200 4
{
T 55400 30200 5 10 1 1 0 0 1
netname=EXT3
}
N 55000 29800 55400 29800 4
{
T 55400 29800 5 10 1 1 0 0 1
netname=EXT4
}
N 55000 29400 55400 29400 4
{
T 55400 29400 5 10 1 1 0 0 1
netname=EXT5
}
C 50200 38900 1 270 0 3.3V-plus-1.sym
N 72500 30600 72300 30600 4
C 70700 30800 1 270 0 resistor-array-8.sym
{
T 71950 30650 5 10 0 0 270 0 1
device=RESISTORPACK_8
T 70700 30800 5 10 0 0 180 0 1
footprint=CTS-742C083
T 70850 29550 5 10 1 1 0 0 1
refdes=R27
T 71800 29500 5 10 1 1 0 0 1
value=180
}
C 73400 30800 1 180 0 led-1.sym
{
T 72600 30200 5 10 0 0 180 0 1
device=LED
T 72600 30000 5 10 0 0 180 0 1
symversion=0.1
T 73400 30800 5 10 0 1 0 0 1
footprint=0603
T 73600 30800 5 10 1 1 180 0 1
refdes=LED12
}
N 70700 30600 70200 30600 4
{
T 70400 30600 5 10 1 1 0 6 1
netname=LED12
}
N 70700 30300 70200 30300 4
{
T 70400 30300 5 10 1 1 0 6 1
netname=ENCR_LED3
}
N 70700 30000 70200 30000 4
{
T 70400 30000 5 10 1 1 0 6 1
netname=ENCR_LED2
}
N 70700 29700 70200 29700 4
{
T 70400 29700 5 10 1 1 0 6 1
netname=ENCR_LED1
}
C 73400 30800 1 270 0 3.3V-plus-1.sym
N 66500 29600 67100 29600 4
{
T 67200 29500 5 10 1 1 0 0 1
netname=LED12
}
N 66500 30000 67100 30000 4
{
T 67200 29900 5 10 1 1 0 0 1
netname=ENCR_LED3
}
N 66500 30400 67100 30400 4
{
T 67200 30300 5 10 1 1 0 0 1
netname=ENCR_LED2
}
N 66500 30800 67100 30800 4
{
T 67200 30700 5 10 1 1 0 0 1
netname=ENCR_LED1
}
C 67900 32900 1 0 0 vishay-form-a-ssr.sym
{
T 70300 34700 5 10 1 1 0 6 1
refdes=U5
T 68300 35700 5 10 0 0 0 0 1
device=VO14642AT
T 68300 35900 5 10 0 0 0 0 1
footprint=SMD-6
}
C 67900 32500 1 0 0 gnd-1.sym
C 71000 33200 1 0 0 connector2-2.sym
{
T 71700 34500 5 10 1 1 0 6 1
refdes=HEAT_SS
T 71300 34450 5 10 0 0 0 0 1
device=CONNECTOR_2
T 71300 34650 5 10 0 0 0 0 1
footprint=JUMPER2
}
N 70600 34200 71000 34200 4
N 71000 34200 71000 34000 4
N 70600 33400 71000 33400 4
N 71000 33400 71000 33600 4
N 51100 32300 51800 32300 4
{
T 51900 32300 5 10 1 1 0 0 1
netname=ENC_B
}
T 50300 39100 9 12 1 0 0 0 1
Address = 0x57
T 63600 31300 9 12 1 0 0 0 1
Address = 0x40
C 48000 30600 1 0 0 nc-left-1.sym
{
T 48000 31000 5 10 0 0 0 0 1
value=NoConnection
T 48000 31400 5 10 0 0 0 0 1
device=DRC_Directive
}
C 51100 30600 1 0 0 nc-right-1.sym
{
T 51200 31100 5 10 0 0 0 0 1
value=NoConnection
T 51200 31300 5 10 0 0 0 0 1
device=DRC_Directive
}
C 51100 31000 1 0 0 nc-right-1.sym
{
T 51200 31500 5 10 0 0 0 0 1
value=NoConnection
T 51200 31700 5 10 0 0 0 0 1
device=DRC_Directive
}
C 51100 30200 1 0 0 nc-right-1.sym
{
T 51200 30700 5 10 0 0 0 0 1
value=NoConnection
T 51200 30900 5 10 0 0 0 0 1
device=DRC_Directive
}
C 48000 30200 1 0 0 nc-left-1.sym
{
T 48000 30600 5 10 0 0 0 0 1
value=NoConnection
T 48000 31000 5 10 0 0 0 0 1
device=DRC_Directive
}
C 48000 29800 1 0 0 nc-left-1.sym
{
T 48000 30200 5 10 0 0 0 0 1
value=NoConnection
T 48000 30600 5 10 0 0 0 0 1
device=DRC_Directive
}
C 48000 34200 1 0 0 nc-left-1.sym
{
T 48000 34600 5 10 0 0 0 0 1
value=NoConnection
T 48000 35000 5 10 0 0 0 0 1
device=DRC_Directive
}
C 51100 34200 1 0 0 nc-right-1.sym
{
T 51200 34700 5 10 0 0 0 0 1
value=NoConnection
T 51200 34900 5 10 0 0 0 0 1
device=DRC_Directive
}
C 51100 33800 1 0 0 nc-right-1.sym
{
T 51200 34300 5 10 0 0 0 0 1
value=NoConnection
T 51200 34500 5 10 0 0 0 0 1
device=DRC_Directive
}
C 48000 33800 1 0 0 nc-left-1.sym
{
T 48000 34200 5 10 0 0 0 0 1
value=NoConnection
T 48000 34600 5 10 0 0 0 0 1
device=DRC_Directive
}
N 47800 32700 48500 32700 4
{
T 47700 32700 5 10 1 1 0 6 1
netname=HEATER_EN_SS
}
N 51100 31500 51800 31500 4
{
T 51900 31500 5 10 1 1 0 0 1
netname=LCD_D6
}
N 47800 31100 48500 31100 4
{
T 47700 31100 5 10 1 1 0 6 1
netname=LCD_D4
}
N 47800 31500 48500 31500 4
{
T 47700 31500 5 10 1 1 0 6 1
netname=LCD_D5
}
N 51100 28300 51800 28300 4
{
T 51900 28300 5 10 1 1 0 0 1
netname=LCD_RS
}
C 74500 35000 1 180 0 nc-right-1.sym
{
T 74400 34500 5 10 0 0 180 0 1
value=NoConnection
T 74400 34300 5 10 0 0 180 0 1
device=DRC_Directive
}
N 77100 34900 77800 34900 4
{
T 77900 34900 5 10 1 1 0 0 1
netname=NRF_CE
}
C 63600 28500 1 270 0 gnd-1.sym
C 60700 30800 1 270 0 3.3V-plus-1.sym
C 60700 32000 1 270 0 3.3V-plus-1.sym
T 60600 32700 9 12 1 0 0 0 1
Address = 0x96
C 74100 23600 1 0 0 resistor-1.sym
{
T 74400 24000 5 10 0 0 0 0 1
device=RESISTOR
T 74100 23600 5 10 0 1 270 0 1
footprint=0603
T 74100 23800 5 10 1 1 0 0 1
refdes=R4
T 74600 23800 5 10 1 1 0 0 1
value=1k
}
C 74100 22800 1 0 0 resistor-1.sym
{
T 74400 23200 5 10 0 0 0 0 1
device=RESISTOR
T 74100 22800 5 10 0 1 270 0 1
footprint=0603
T 74100 23000 5 10 1 1 0 0 1
refdes=R5
T 74600 23000 5 10 1 1 0 0 1
value=1k
}
C 72700 20900 1 90 0 capacitor-1.sym
{
T 72000 21100 5 10 0 0 90 0 1
device=CAPACITOR
T 72400 20900 5 10 1 1 90 0 1
refdes=C12
T 71800 21100 5 10 0 0 90 0 1
symversion=0.1
T 72700 20900 5 10 1 1 90 0 1
value=10n
T 72700 20900 5 10 0 1 0 0 1
footprint=0603
}
N 72500 21800 72100 21800 4
C 71700 20900 1 90 0 capacitor-1.sym
{
T 71000 21100 5 10 0 0 90 0 1
device=CAPACITOR
T 71400 20900 5 10 1 1 90 0 1
refdes=C11
T 70800 21100 5 10 0 0 90 0 1
symversion=0.1
T 71700 20900 5 10 1 1 90 0 1
value=10n
T 71700 20900 5 10 0 1 0 0 1
footprint=0603
}
C 70800 20900 1 90 0 capacitor-1.sym
{
T 70100 21100 5 10 0 0 90 0 1
device=CAPACITOR
T 70500 20900 5 10 1 1 90 0 1
refdes=C10
T 69900 21100 5 10 0 0 90 0 1
symversion=0.1
T 70800 20900 5 10 1 1 90 0 1
value=10n
T 70800 20900 5 10 0 1 0 0 1
footprint=0603
}
C 70500 20600 1 0 0 gnd-1.sym
C 71400 20600 1 0 0 gnd-1.sym
C 72400 20600 1 0 0 gnd-1.sym
C 56900 32900 1 180 0 npn-2.sym
{
T 56300 32400 5 10 0 0 180 0 1
device=NPN_TRANSISTOR
T 56400 32600 5 10 1 1 180 0 1
refdes=Q2
T 56900 32900 5 10 0 0 0 0 1
footprint=TO92
}
C 74700 23400 1 270 0 gnd-1.sym
C 77600 23900 1 270 0 3.3V-plus-1.sym
N 78100 22900 77600 22900 4
{
T 78150 22850 5 10 1 1 0 0 1
netname=ENC_LED1
}
N 78100 22500 77600 22500 4
{
T 78150 22450 5 10 1 1 0 0 1
netname=ENC_LED2
}
N 78100 22100 77600 22100 4
{
T 78150 22050 5 10 1 1 0 0 1
netname=ENC_LED3
}
N 72300 30300 73900 30300 4
{
T 73900 30300 5 10 1 1 0 0 1
netname=ENC_LED3
}
N 72300 30000 73900 30000 4
{
T 73900 30000 5 10 1 1 0 0 1
netname=ENC_LED2
}
N 72300 29700 73900 29700 4
{
T 73900 29700 5 10 1 1 0 0 1
netname=ENC_LED1
}
C 67500 32800 1 0 0 bc817.sym
{
T 68100 33300 5 10 0 0 0 0 1
device=NPN_TRANSISTOR
T 67600 33700 5 10 1 1 0 0 1
refdes=Q3
T 68100 34100 5 10 0 1 0 0 1
footprint=SOT23
T 67100 33000 5 10 1 1 0 0 1
model=BC817
}
N 66600 33300 65900 33300 4
{
T 65900 33300 5 10 1 1 0 6 1
netname=HEATER_EN_SS
}
C 67800 35100 1 0 0 5V-plus-1.sym
C 68100 34200 1 90 0 resistor-1.sym
{
T 67700 34500 5 10 0 0 90 0 1
device=RESISTOR
T 67800 34400 5 10 1 1 90 0 1
refdes=R3
T 67800 34900 5 10 1 1 90 0 1
value=100
T 68100 34200 5 10 0 1 90 0 1
footprint=0603
}
C 66600 33200 1 0 0 resistor-1.sym
{
T 66900 33600 5 10 0 0 0 0 1
device=RESISTOR
T 66800 33500 5 10 1 1 0 0 1
refdes=R13
T 67200 33500 5 10 1 1 0 0 1
value=1k
T 66600 33200 5 10 0 1 0 0 1
footprint=0603
}
T 46700 30300 9 12 1 0 0 0 1
eMMC pins
T 51800 30600 9 12 1 0 0 0 1
eMMC pins
C 50100 21200 1 90 1 led-1.sym
{
T 49500 20400 5 10 0 0 90 6 1
device=LED
T 50300 21100 5 10 1 1 90 6 1
refdes=LED_RSSI
T 49300 20400 5 10 0 0 90 6 1
symversion=0.1
T 50100 21200 5 10 0 0 0 0 1
footprint=0603
}
C 50000 19400 1 90 0 resistor-1.sym
{
T 49600 19700 5 10 0 0 90 0 1
device=RESISTOR
T 49800 19400 5 10 1 1 90 0 1
refdes=R28
T 49800 19900 5 10 1 1 90 0 1
value=220
T 50000 19400 5 10 0 1 0 0 1
footprint=0603
}
C 49800 19100 1 0 0 gnd-1.sym
N 49900 21200 49900 21700 4
{
T 49800 21200 5 10 1 1 90 0 1
netname=XBEE_RSSI
}
N 53200 21600 52700 21600 4
{
T 52600 21600 5 10 1 1 0 6 1
netname=XBEE_RSSI
}
C 75100 24100 1 90 0 resistor-1.sym
{
T 74700 24400 5 10 0 0 90 0 1
device=RESISTOR
T 75100 24100 5 10 0 1 0 0 1
footprint=0603
T 74900 24100 5 10 1 1 90 0 1
refdes=R31
T 74900 24600 5 10 1 1 90 0 1
value=1k
}
C 74800 25000 1 0 0 3.3V-plus-1.sym
N 75000 24100 75000 23700 4
N 75000 22500 75000 22900 4
