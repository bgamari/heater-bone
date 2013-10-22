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
C 72900 25900 1 0 0 bbb-p9.sym
{
T 75300 35700 5 10 1 1 0 6 1
refdes=P9
T 73300 35900 5 10 0 0 0 0 1
device=BeagleBone
T 73300 36100 5 10 0 0 0 0 1
footprint=CONNECTOR 46 2
}
C 45100 18700 0 0 0 title-D.sym
C 57500 28600 1 0 0 ad7411.sym
{
T 59900 32700 5 10 1 1 0 6 1
refdes=U3
T 58700 29600 5 10 0 0 0 0 1
device=ADT7411
T 58700 29800 5 10 0 0 0 0 1
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
C 66300 32350 1 0 0 encoder.sym
{
T 66700 34950 5 10 0 0 0 0 1
footprint=encoder-with-switch
T 68750 33700 5 10 1 1 0 6 1
refdes=ENC
T 67500 33000 5 10 0 0 0 0 1
device=Rotary encoder
}
C 48200 34800 1 270 0 gnd-1.sym
C 69300 32750 1 90 0 gnd-1.sym
C 75900 35100 1 90 0 gnd-1.sym
C 72700 35300 1 270 0 gnd-1.sym
C 73000 34600 1 90 0 3.3V-plus-1.sym
C 75600 35000 1 270 0 3.3V-plus-1.sym
C 73000 33800 1 90 0 5V-plus-1.sym
C 75600 34200 1 270 0 5V-plus-1.sym
C 75900 26700 1 90 0 gnd-1.sym
C 75900 26300 1 90 0 gnd-1.sym
C 72700 26500 1 270 0 gnd-1.sym
C 72700 26900 1 270 0 gnd-1.sym
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
N 73000 31600 72300 31600 4
{
T 72200 31600 5 10 1 1 0 6 1
netname=I2C2_SCL
}
N 75600 31600 76300 31600 4
{
T 76400 31600 5 10 1 1 0 0 1
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
C 58800 28400 1 0 0 gnd-1.sym
C 51600 34600 1 90 0 gnd-1.sym
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
C 51400 38000 1 180 0 resistor-1.sym
{
T 51100 37600 5 10 0 0 180 0 1
device=RESISTOR
T 51200 37700 5 10 1 1 180 0 1
refdes=R5
T 50900 37700 5 10 1 1 180 0 1
value=1k
T 51400 38000 5 10 0 1 0 0 1
footprint=0603
}
C 51400 38100 1 270 0 3.3V-plus-1.sym
N 50500 37900 50200 37900 4
C 50500 37400 1 90 0 gnd-1.sym
C 52300 38300 1 270 0 connector2-2.sym
{
T 52300 37400 5 10 1 1 0 0 1
refdes=ADDR1
T 53550 38000 5 10 0 0 270 0 1
device=CONNECTOR_2
T 53750 38000 5 10 0 0 270 0 1
footprint=JUMPER2
}
N 50200 38300 52700 38300 4
C 53500 38700 1 90 0 connector2-2.sym
{
T 52300 39600 5 10 1 1 180 6 1
refdes=ADDR0
T 52250 39000 5 10 0 0 90 0 1
device=CONNECTOR_2
T 52050 39000 5 10 0 0 90 0 1
footprint=JUMPER2
}
N 50200 38700 52700 38700 4
C 50500 39600 1 270 0 resistor-1.sym
{
T 50900 39300 5 10 0 0 270 0 1
device=RESISTOR
T 50800 39400 5 10 1 1 270 0 1
refdes=R3
T 50800 39100 5 10 1 1 270 0 1
value=1k
T 50500 39600 5 10 0 1 0 0 1
footprint=0603
}
C 51200 39200 1 270 0 resistor-1.sym
{
T 51600 38900 5 10 0 0 270 0 1
device=RESISTOR
T 51500 39000 5 10 1 1 270 0 1
refdes=R4
T 51500 38700 5 10 1 1 270 0 1
value=1k
T 51200 39200 5 10 0 1 0 0 1
footprint=0603
}
C 51100 39200 1 0 0 3.3V-plus-1.sym
C 50400 39600 1 0 0 3.3V-plus-1.sym
C 53400 38600 1 90 0 gnd-1.sym
C 53400 38200 1 90 0 gnd-1.sym
N 66100 32850 66400 32850 4
{
T 66000 32850 5 10 1 1 0 6 1
netname=ENC_B
}
N 69300 33250 69000 33250 4
{
T 69400 33250 5 10 1 1 0 0 1
netname=ENC_SW
}
N 66100 33250 66400 33250 4
{
T 66000 33250 5 10 1 1 0 6 1
netname=ENC_A
}
C 58700 32900 1 0 0 3.3V-plus-1.sym
C 63000 21200 1 0 0 3.3V-plus-1.sym
C 62900 21500 1 180 0 gnd-1.sym
C 65800 25200 1 0 0 tca6507.sym
{
T 67600 28900 5 10 1 1 0 6 1
refdes=U4
T 66700 27400 5 10 0 0 0 0 1
device=TCA6507
}
N 60200 31400 60800 31400 4
{
T 60900 31400 5 10 1 1 0 0 1
netname=SPI1_MISO
}
N 60200 31000 60800 31000 4
{
T 60900 31000 5 10 1 1 0 0 1
netname=SPI1_MOSI
}
N 60200 32200 60800 32200 4
{
T 60900 32200 5 10 1 1 0 0 1
netname=ADC_\_CS\_
}
N 60200 31800 60800 31800 4
{
T 60900 31800 5 10 1 1 0 0 1
netname=ADC_INT
}
N 60200 30600 60800 30600 4
{
T 60900 30600 5 10 1 1 0 0 1
netname=SPI1_SCLK
}
N 65900 28000 65200 28000 4
{
T 65100 28000 5 10 1 1 0 6 1
netname=I2C2_SDA
}
N 65900 28400 65200 28400 4
{
T 65100 28400 5 10 1 1 0 6 1
netname=I2C2_SCL
}
N 65900 27600 65200 27600 4
{
T 65100 27600 5 10 1 1 0 6 1
netname=LEDS_EN
}
C 66700 29100 1 0 0 3.3V-plus-1.sym
C 66800 25000 1 0 0 gnd-1.sym
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
N 67900 28400 68100 28400 4
C 68100 28300 1 0 0 resistor-1.sym
{
T 68400 28700 5 10 0 0 0 0 1
device=RESISTOR
T 68300 28500 5 10 1 1 0 0 1
refdes=R22
T 68600 28500 5 10 1 1 0 0 1
value=1k
T 68100 28300 5 10 0 1 0 0 1
footprint=0603
}
C 68100 27900 1 0 0 resistor-1.sym
{
T 68400 28300 5 10 0 0 0 0 1
device=RESISTOR
T 68300 28100 5 10 1 1 0 0 1
refdes=R23
T 68600 28100 5 10 1 1 0 0 1
value=1k
T 68100 27900 5 10 0 1 0 0 1
footprint=0603
}
C 68100 27500 1 0 0 resistor-1.sym
{
T 68400 27900 5 10 0 0 0 0 1
device=RESISTOR
T 68300 27700 5 10 1 1 0 0 1
refdes=R24
T 68600 27700 5 10 1 1 0 0 1
value=1k
T 68100 27500 5 10 0 1 0 0 1
footprint=0603
}
N 67900 28000 68100 28000 4
N 67900 27600 68100 27600 4
C 68100 27100 1 0 0 resistor-1.sym
{
T 68400 27500 5 10 0 0 0 0 1
device=RESISTOR
T 68300 27300 5 10 1 1 0 0 1
refdes=R25
T 68600 27300 5 10 1 1 0 0 1
value=1k
T 68100 27100 5 10 0 1 0 0 1
footprint=0603
}
N 67900 27200 68100 27200 4
N 67900 26800 68100 26800 4
C 68100 26700 1 0 0 resistor-1.sym
{
T 68400 27100 5 10 0 0 0 0 1
device=RESISTOR
T 68300 26900 5 10 1 1 0 0 1
refdes=R26
T 68600 26900 5 10 1 1 0 0 1
value=1k
T 68100 26700 5 10 0 1 0 0 1
footprint=0603
}
C 68100 26300 1 0 0 resistor-1.sym
{
T 68400 26700 5 10 0 0 0 0 1
device=RESISTOR
T 68300 26500 5 10 1 1 0 0 1
refdes=R27
T 68600 26500 5 10 1 1 0 0 1
value=1k
T 68100 26300 5 10 0 1 0 0 1
footprint=0603
}
C 68100 25900 1 0 0 resistor-1.sym
{
T 68400 26300 5 10 0 0 0 0 1
device=RESISTOR
T 68300 26100 5 10 1 1 0 0 1
refdes=R28
T 68600 26100 5 10 1 1 0 0 1
value=0
T 68100 25900 5 10 0 1 0 0 1
footprint=0603
}
N 67900 26400 68100 26400 4
N 67900 26000 68100 26000 4
N 69000 26000 69300 26000 4
{
T 69400 26000 5 10 1 1 0 0 1
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
C 56500 29300 1 0 0 resistor-1.sym
{
T 56800 29700 5 10 0 0 0 0 1
device=RESISTOR
T 56700 29500 5 10 1 1 0 0 1
refdes=R17
T 56500 29300 5 10 0 1 0 0 1
footprint=0603
}
C 56500 29200 1 90 0 3.3V-plus-1.sym
N 57400 29400 57600 29400 4
C 65500 35400 1 0 0 resistor-1.sym
{
T 65800 35800 5 10 0 0 0 0 1
device=RESISTOR
T 65700 35700 5 10 1 1 0 0 1
refdes=R21
T 66000 35700 5 10 1 1 0 0 1
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
N 51300 34700 51100 34700 4
N 69000 28000 69500 28000 4
N 69000 28400 69500 28400 4
N 51100 31900 51800 31900 4
{
T 51900 31900 5 10 1 1 0 0 1
netname=ENC_A
}
N 47800 31500 48500 31500 4
{
T 47700 31500 5 10 1 1 0 6 1
netname=ENC_B
}
N 47800 31900 48500 31900 4
{
T 47700 31900 5 10 1 1 0 6 1
netname=ENC_SW
}
N 73000 29600 72300 29600 4
{
T 72200 29600 5 10 1 1 0 6 1
netname=SPI1_MOSI
}
N 73000 29200 72300 29200 4
{
T 72200 29200 5 10 1 1 0 6 1
netname=SPI1_SCLK
}
N 75600 30000 76300 30000 4
{
T 76400 30000 5 10 1 1 0 0 1
netname=ADC_\_CS\_
}
N 75600 29600 76300 29600 4
{
T 76400 29600 5 10 1 1 0 0 1
netname=SPI1_MISO
}
C 56500 29700 1 0 0 resistor-1.sym
{
T 56800 30100 5 10 0 0 0 0 1
device=RESISTOR
T 56700 29900 5 10 1 1 0 0 1
refdes=R16
T 56500 29700 5 10 0 1 0 0 1
footprint=0603
}
C 56500 29600 1 90 0 3.3V-plus-1.sym
N 57400 29800 57600 29800 4
C 56500 30100 1 0 0 resistor-1.sym
{
T 56800 30500 5 10 0 0 0 0 1
device=RESISTOR
T 56700 30300 5 10 1 1 0 0 1
refdes=R15
T 56500 30100 5 10 0 1 0 0 1
footprint=0603
}
C 56500 30000 1 90 0 3.3V-plus-1.sym
N 57400 30200 57600 30200 4
C 56500 30500 1 0 0 resistor-1.sym
{
T 56800 30900 5 10 0 0 0 0 1
device=RESISTOR
T 56700 30700 5 10 1 1 0 0 1
refdes=R14
T 56500 30500 5 10 0 1 0 0 1
footprint=0603
}
C 56500 30400 1 90 0 3.3V-plus-1.sym
N 57400 30600 57600 30600 4
C 56500 30900 1 0 0 resistor-1.sym
{
T 56800 31300 5 10 0 0 0 0 1
device=RESISTOR
T 56700 31100 5 10 1 1 0 0 1
refdes=R13
T 56500 30900 5 10 0 1 0 0 1
footprint=0603
}
C 56500 30800 1 90 0 3.3V-plus-1.sym
N 57400 31000 57600 31000 4
C 56500 31300 1 0 0 resistor-1.sym
{
T 56800 31700 5 10 0 0 0 0 1
device=RESISTOR
T 56700 31500 5 10 1 1 0 0 1
refdes=R12
T 56500 31300 5 10 0 1 0 0 1
footprint=0603
}
C 56500 31200 1 90 0 3.3V-plus-1.sym
N 57400 31400 57600 31400 4
C 56500 31700 1 0 0 resistor-1.sym
{
T 56800 32100 5 10 0 0 0 0 1
device=RESISTOR
T 56700 31900 5 10 1 1 0 0 1
refdes=R11
T 56500 31700 5 10 0 1 0 0 1
footprint=0603
}
C 56500 31600 1 90 0 3.3V-plus-1.sym
N 57400 31800 57600 31800 4
C 56500 32100 1 0 0 resistor-1.sym
{
T 56800 32500 5 10 0 0 0 0 1
device=RESISTOR
T 56700 32300 5 10 1 1 0 0 1
refdes=R10
T 56500 32100 5 10 0 1 0 0 1
footprint=0603
}
C 56500 32000 1 90 0 3.3V-plus-1.sym
N 57400 32200 57600 32200 4
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
footprint=none
T 63300 22400 5 10 1 1 180 0 1
value=10k
}
N 63600 21200 63600 22100 4
C 63700 23300 1 180 0 gnd-1.sym
C 62800 22700 1 270 0 gnd-1.sym
N 66400 35500 66700 35500 4
C 64500 21500 1 180 0 gnd-1.sym
N 51100 30700 51800 30700 4
{
T 51900 30700 5 10 1 1 0 0 1
netname=LCD_RS
}
N 47800 30700 48500 30700 4
{
T 47700 30700 5 10 1 1 0 6 1
netname=LCD_E
}
N 51100 30300 51800 30300 4
{
T 51900 30300 5 10 1 1 0 0 1
netname=LCD_D0
}
N 47800 30300 48500 30300 4
{
T 47700 30300 5 10 1 1 0 6 1
netname=LCD_D1
}
N 47800 29900 48500 29900 4
{
T 47700 29900 5 10 1 1 0 6 1
netname=LCD_D3
}
N 51100 29900 51800 29900 4
{
T 51900 29900 5 10 1 1 0 0 1
netname=LCD_D2
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
C 57600 40200 1 0 0 3.3V-plus-1.sym
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
N 48500 34300 47800 34300 4
{
T 47700 34300 5 10 1 1 0 6 1
netname=BTN4
}
N 48500 33900 47800 33900 4
{
T 47700 33900 5 10 1 1 0 6 1
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
C 59900 34800 1 90 0 resistor-1.sym
{
T 59500 35100 5 10 0 0 90 0 1
device=RESISTOR
T 59700 34800 5 10 1 1 90 0 1
refdes=R18
T 59700 35300 5 10 1 1 90 0 1
value=1k
T 59900 34800 5 10 0 1 0 0 1
footprint=0603
}
N 59800 34800 59800 34200 4
{
T 59800 34100 5 10 1 1 90 6 1
netname=SPI1_MISO
}
C 59600 35700 1 0 0 3.3V-plus-1.sym
C 60500 34800 1 90 0 resistor-1.sym
{
T 60100 35100 5 10 0 0 90 0 1
device=RESISTOR
T 60300 34800 5 10 1 1 90 0 1
refdes=R19
T 60300 35300 5 10 1 1 90 0 1
value=1k
T 60500 34800 5 10 0 1 0 0 1
footprint=0603
}
N 60400 34800 60400 34200 4
{
T 60400 34100 5 10 1 1 90 6 1
netname=SPI1_MOSI
}
C 60200 35700 1 0 0 3.3V-plus-1.sym
C 61100 34800 1 90 0 resistor-1.sym
{
T 60700 35100 5 10 0 0 90 0 1
device=RESISTOR
T 60900 34800 5 10 1 1 90 0 1
refdes=R20
T 60900 35300 5 10 1 1 90 0 1
value=1k
T 61100 34800 5 10 0 1 0 0 1
footprint=0603
}
C 60800 35700 1 0 0 3.3V-plus-1.sym
N 61000 34800 61000 34200 4
{
T 61000 34100 5 10 1 1 90 6 1
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
value=10u
T 50100 22700 5 10 0 1 0 0 1
footprint=0603
}
C 49800 22400 1 0 0 gnd-1.sym
C 57800 33100 1 90 0 capacitor-1.sym
{
T 57100 33300 5 10 0 0 90 0 1
device=CAPACITOR
T 56900 33300 5 10 0 0 90 0 1
symversion=0.1
T 57500 33200 5 10 1 1 90 0 1
refdes=C5
T 57800 33100 5 10 1 1 90 0 1
value=100n
T 57800 33100 5 10 0 1 0 0 1
footprint=0603
}
C 57400 34000 1 0 0 3.3V-plus-1.sym
C 57500 32800 1 0 0 gnd-1.sym
C 57200 33100 1 90 0 capacitor-1.sym
{
T 56500 33300 5 10 0 0 90 0 1
device=CAPACITOR
T 56300 33300 5 10 0 0 90 0 1
symversion=0.1
T 56900 33200 5 10 1 1 90 0 1
refdes=C4
T 57200 33100 5 10 1 1 90 0 1
value=1u
T 57200 33100 5 10 0 1 0 0 1
footprint=0603
}
C 56800 34000 1 0 0 3.3V-plus-1.sym
C 56900 32800 1 0 0 gnd-1.sym
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
C 65900 29300 1 90 0 capacitor-1.sym
{
T 65200 29500 5 10 0 0 90 0 1
device=CAPACITOR
T 65000 29500 5 10 0 0 90 0 1
symversion=0.1
T 65600 29400 5 10 1 1 90 0 1
refdes=C8
T 65900 29300 5 10 1 1 90 0 1
value=1u
T 65900 29300 5 10 0 1 0 0 1
footprint=0603
}
C 65500 30200 1 0 0 3.3V-plus-1.sym
C 65600 29000 1 0 0 gnd-1.sym
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
N 73000 30000 72300 30000 4
{
T 72200 30000 5 10 1 1 0 6 1
netname=ADC_INT
}
C 51000 23600 1 90 1 led-1.sym
{
T 50400 22800 5 10 0 0 90 6 1
device=LED
T 51200 23500 5 10 1 1 90 6 1
refdes=LED1
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
T 67300 35500 5 10 1 1 0 0 1
refdes=Q1
T 67300 36300 5 10 0 1 0 0 1
footprint=SOT23
}
C 54400 29400 1 0 0 header16-1.sym
{
T 54450 28350 5 10 0 1 0 0 1
device=HEADER16
T 55000 32700 5 10 1 1 0 0 1
refdes=EXT
T 54400 29400 5 10 0 0 0 0 1
footprint=HEADER16_2
}
N 55800 32400 57500 32400 4
N 57500 32400 57500 32200 4
N 55800 32000 57500 32000 4
N 57500 32000 57500 31800 4
N 55800 31600 57500 31600 4
N 57500 31600 57500 31400 4
N 55800 31200 57500 31200 4
N 57500 31200 57500 31000 4
N 55800 30800 57500 30800 4
N 57500 30800 57500 30600 4
N 55800 30400 57500 30400 4
N 57500 30400 57500 30200 4
N 55800 30000 57500 30000 4
N 57500 30000 57500 29800 4
N 55800 29600 57500 29600 4
N 57500 29600 57500 29400 4
C 54100 32500 1 270 0 gnd-1.sym
C 54100 32100 1 270 0 gnd-1.sym
C 54100 31700 1 270 0 gnd-1.sym
C 54100 31300 1 270 0 gnd-1.sym
C 54100 30900 1 270 0 gnd-1.sym
C 54100 30500 1 270 0 gnd-1.sym
C 54100 30100 1 270 0 gnd-1.sym
C 54100 29700 1 270 0 gnd-1.sym
C 67500 38800 1 0 0 5V-plus-1.sym
C 63200 33400 1 90 0 resistor-1.sym
{
T 62800 33700 5 10 0 0 90 0 1
device=RESISTOR
T 63200 33400 5 10 0 1 0 0 1
footprint=0603
T 63000 33400 5 10 1 1 90 0 1
refdes=R30
T 63000 33900 5 10 1 1 90 0 1
value=1k
}
N 63100 33400 63100 32800 4
{
T 63100 32700 5 10 1 1 90 6 1
netname=ENC_B
}
C 62900 34300 1 0 0 3.3V-plus-1.sym
C 63800 33400 1 90 0 resistor-1.sym
{
T 63400 33700 5 10 0 0 90 0 1
device=RESISTOR
T 63800 33400 5 10 0 1 0 0 1
footprint=0603
T 63600 33400 5 10 1 1 90 0 1
refdes=R31
T 63600 33900 5 10 1 1 90 0 1
value=1k
}
C 63500 34300 1 0 0 3.3V-plus-1.sym
N 63700 33400 63700 32800 4
{
T 63700 32700 5 10 1 1 90 6 1
netname=ENC_A
}
C 64400 33400 1 90 0 resistor-1.sym
{
T 64000 33700 5 10 0 0 90 0 1
device=RESISTOR
T 64400 33400 5 10 0 1 0 0 1
footprint=0603
T 64200 33400 5 10 1 1 90 0 1
refdes=R32
T 64200 33900 5 10 1 1 90 0 1
value=1k
}
N 64300 33400 64300 32800 4
{
T 64300 32700 5 10 1 1 90 6 1
netname=ENC_SW
}
C 64100 34300 1 0 0 3.3V-plus-1.sym
N 51100 33900 51800 33900 4
{
T 51900 33900 5 10 1 1 0 0 1
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
C 58200 39200 1 90 0 smt-pushbutton-no.sym
{
T 57600 39600 5 10 1 1 90 0 1
refdes=BTN2
T 57600 39600 5 10 0 0 90 0 1
device=SWITCH_PUSHBUTTON_NO
T 58200 39200 5 10 0 1 90 0 1
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
N 51100 34300 51800 34300 4
{
T 51900 34300 5 10 1 1 0 0 1
netname=BTN3
}
N 69400 38800 69400 38850 4
N 69000 27600 69500 27600 4
C 70900 28800 1 180 0 rgb-led.sym
{
T 70100 28200 5 10 0 0 180 0 1
device=LED
T 70250 27250 5 10 1 1 180 0 1
refdes=RGB_LED
T 70100 28000 5 10 0 0 180 0 1
symversion=0.1
T 70900 28800 5 10 0 0 180 6 1
footprint=PLCC4
}
N 69500 28400 69500 28300 4
N 69500 27600 69500 27700 4
C 70600 28200 1 270 0 3.3V-plus-1.sym
C 71500 27400 1 180 0 led-1.sym
{
T 70700 26800 5 10 0 0 180 0 1
device=LED
T 71700 27400 5 10 1 1 180 0 1
refdes=LED2
T 70700 26600 5 10 0 0 180 0 1
symversion=0.1
T 71500 27400 5 10 0 1 0 0 1
footprint=0603
}
N 70600 27200 69000 27200 4
C 71500 27000 1 180 0 led-1.sym
{
T 70700 26400 5 10 0 0 180 0 1
device=LED
T 71700 27000 5 10 1 1 180 0 1
refdes=LED3
T 70700 26200 5 10 0 0 180 0 1
symversion=0.1
T 71500 27000 5 10 0 1 0 0 1
footprint=0603
}
N 70600 26800 69000 26800 4
C 71500 26600 1 180 0 led-1.sym
{
T 70700 26000 5 10 0 0 180 0 1
device=LED
T 71700 26600 5 10 1 1 180 0 1
refdes=LED4
T 70700 25800 5 10 0 0 180 0 1
symversion=0.1
T 71500 26600 5 10 0 1 0 0 1
footprint=0603
}
N 70600 26400 69000 26400 4
C 71500 27400 1 270 0 3.3V-plus-1.sym
C 71500 27000 1 270 0 3.3V-plus-1.sym
C 71500 26600 1 270 0 3.3V-plus-1.sym
T 72100 19400 9 12 1 0 0 0 1
Heater control cape for BeagleBone
N 47800 29500 48500 29500 4
{
T 47700 29500 5 10 1 1 0 6 1
netname=LCD_D5
}
N 47800 29100 48500 29100 4
{
T 47700 29100 5 10 1 1 0 6 1
netname=LCD_D7
}
N 51100 29500 51800 29500 4
{
T 51900 29500 5 10 1 1 0 0 1
netname=LCD_D4
}
N 51100 29100 51800 29100 4
{
T 51900 29100 5 10 1 1 0 0 1
netname=LCD_D6
}
