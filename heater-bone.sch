v 20121203 2
C 48400 25400 1 0 0 bbb-p8.sym
{
T 50200 35200 5 10 1 1 0 6 1
refdes=J?
T 48800 35400 5 10 0 0 0 0 1
device=BeagleBone
T 48800 35600 5 10 0 0 0 0 1
footprint=HEADER 23 2
}
C 72900 25900 1 0 0 bbb-p9.sym
{
T 75300 35700 5 10 1 1 0 6 1
refdes=J?
T 73300 35900 5 10 0 0 0 0 1
device=BeagleBone
T 73300 36100 5 10 0 0 0 0 1
footprint=HEADER 23 2
}
C 45100 18700 0 0 0 title-D.sym
C 57500 28600 1 0 0 ad7411.sym
{
T 59900 32700 5 10 1 1 0 6 1
refdes=U?
T 58700 29600 5 10 0 0 0 0 1
device=ADT7411
T 58700 29800 5 10 0 0 0 0 1
footprint=SOP16
}
C 62300 18100 1 0 0 lcd.sym
{
T 69900 21000 5 10 1 1 0 6 1
refdes=LCD
T 63500 20700 5 10 0 0 0 0 1
device=LCD
T 63500 20900 5 10 0 0 0 0 1
footprint=DIP
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
}
C 65500 30900 1 0 0 encoder.sym
{
T 67900 33100 5 10 1 1 0 6 1
refdes=ENC
T 66700 31550 5 10 0 0 0 0 1
device=Rotary encoder
T 66700 31750 5 10 0 0 0 0 1
footprint=encoder
}
C 48200 34800 1 270 0 gnd-1.sym
C 68500 31300 1 90 0 gnd-1.sym
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
refdes=U?
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
N 51100 34300 51800 34300 4
{
T 51900 34300 5 10 1 1 0 0 1
netname=HEATER_EN
}
C 66700 35000 1 0 0 npn-1.sym
{
T 67300 35500 5 10 0 0 0 0 1
device=NPN_TRANSISTOR
T 67300 35500 5 10 1 1 0 0 1
refdes=Q?
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
refdes=D?
}
N 67200 38800 67200 37800 4
N 67200 36900 67200 36000 4
N 67200 36000 68100 36000 4
C 67100 34700 1 0 0 gnd-1.sym
C 67000 38800 1 0 0 3.3V-plus-1.sym
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
refdes=R?
T 47700 39200 5 10 1 1 90 0 1
value=10k
}
C 48000 37400 1 90 0 resistor-1.sym
{
T 47600 37700 5 10 0 0 90 0 1
device=RESISTOR
T 47700 37600 5 10 1 1 90 0 1
refdes=R?
T 47700 37900 5 10 1 1 90 0 1
value=10k
}
C 47700 39600 1 0 0 3.3V-plus-1.sym
C 48100 37400 1 180 0 3.3V-plus-1.sym
C 51400 38000 1 180 0 resistor-1.sym
{
T 51100 37600 5 10 0 0 180 0 1
device=RESISTOR
T 51200 37700 5 10 1 1 180 0 1
refdes=R?
T 50900 37700 5 10 1 1 180 0 1
value=1k
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
footprint=SIP2N
}
N 50200 38300 52700 38300 4
C 53500 38700 1 90 0 connector2-2.sym
{
T 52300 39600 5 10 1 1 180 6 1
refdes=ADDR0
T 52250 39000 5 10 0 0 90 0 1
device=CONNECTOR_2
T 52050 39000 5 10 0 0 90 0 1
footprint=SIP2N
}
N 50200 38700 52700 38700 4
C 50500 39600 1 270 0 resistor-1.sym
{
T 50900 39300 5 10 0 0 270 0 1
device=RESISTOR
T 50800 39400 5 10 1 1 270 0 1
refdes=R?
T 50800 39100 5 10 1 1 270 0 1
value=1k
}
C 51200 39200 1 270 0 resistor-1.sym
{
T 51600 38900 5 10 0 0 270 0 1
device=RESISTOR
T 51500 39000 5 10 1 1 270 0 1
refdes=R?
T 51500 38700 5 10 1 1 270 0 1
value=1k
}
C 51100 39200 1 0 0 3.3V-plus-1.sym
C 50400 39600 1 0 0 3.3V-plus-1.sym
C 53400 38600 1 90 0 gnd-1.sym
C 53400 38200 1 90 0 gnd-1.sym
N 65300 32200 65600 32200 4
{
T 65200 32200 5 10 1 1 0 6 1
netname=ENC_B
}
N 65300 31800 65600 31800 4
{
T 65200 31800 5 10 1 1 0 6 1
netname=ENC_C
}
N 68500 32600 68200 32600 4
{
T 68600 32600 5 10 1 1 0 0 1
netname=ENC_SW
}
N 68500 32200 68200 32200 4
{
T 68600 32200 5 10 1 1 0 0 1
netname=ENC_GRN
}
N 65300 32600 65600 32600 4
{
T 65200 32600 5 10 1 1 0 6 1
netname=ENC_A
}
N 68500 31800 68200 31800 4
{
T 68600 31800 5 10 1 1 0 0 1
netname=ENC_RED
}
C 58700 32900 1 0 0 3.3V-plus-1.sym
C 63000 21200 1 0 0 3.3V-plus-1.sym
C 62900 21500 1 180 0 gnd-1.sym
C 65800 25200 1 0 0 tca6507.sym
{
T 67600 28900 5 10 1 1 0 6 1
refdes=U?
T 66700 27400 5 10 0 0 0 0 1
device=TCA6507
T 66700 27600 5 10 0 0 0 0 1
footprint=TSSOP-65P-640L1-14N
}
N 60200 31400 60800 31400 4
{
T 60900 31400 5 10 1 1 0 0 1
netname=ADC_MISO
}
N 60200 31000 60800 31000 4
{
T 60900 31000 5 10 1 1 0 0 1
netname=ADC_MOSI
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
netname=ADC_SCLK
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
refdes=R?
T 68600 28500 5 10 1 1 0 0 1
value=1k
}
C 68100 27900 1 0 0 resistor-1.sym
{
T 68400 28300 5 10 0 0 0 0 1
device=RESISTOR
T 68300 28100 5 10 1 1 0 0 1
refdes=R?
T 68600 28100 5 10 1 1 0 0 1
value=1k
}
C 68100 27500 1 0 0 resistor-1.sym
{
T 68400 27900 5 10 0 0 0 0 1
device=RESISTOR
T 68300 27700 5 10 1 1 0 0 1
refdes=R?
T 68600 27700 5 10 1 1 0 0 1
value=1k
}
N 67900 28000 68100 28000 4
N 67900 27600 68100 27600 4
C 68100 27100 1 0 0 resistor-1.sym
{
T 68400 27500 5 10 0 0 0 0 1
device=RESISTOR
T 68300 27300 5 10 1 1 0 0 1
refdes=R?
T 68600 27300 5 10 1 1 0 0 1
value=1k
}
N 67900 27200 68100 27200 4
N 67900 26800 68100 26800 4
C 68100 26700 1 0 0 resistor-1.sym
{
T 68400 27100 5 10 0 0 0 0 1
device=RESISTOR
T 68300 26900 5 10 1 1 0 0 1
refdes=R?
T 68600 26900 5 10 1 1 0 0 1
value=1k
}
C 68100 26300 1 0 0 resistor-1.sym
{
T 68400 26700 5 10 0 0 0 0 1
device=RESISTOR
T 68300 26500 5 10 1 1 0 0 1
refdes=R?
T 68600 26500 5 10 1 1 0 0 1
value=1k
}
C 68100 25900 1 0 0 resistor-1.sym
{
T 68400 26300 5 10 0 0 0 0 1
device=RESISTOR
T 68300 26100 5 10 1 1 0 0 1
refdes=R?
T 68600 26100 5 10 1 1 0 0 1
value=0
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
C 55100 29300 1 0 0 resistor-1.sym
{
T 55400 29700 5 10 0 0 0 0 1
device=RESISTOR
T 55300 29500 5 10 1 1 0 0 1
refdes=R?
T 55600 29500 5 10 1 1 0 0 1
value=1k
}
C 55100 29200 1 90 0 3.3V-plus-1.sym
N 56000 29400 57600 29400 4
C 57100 32600 1 180 0 connector8-2.sym
{
T 56400 28900 5 10 1 1 180 6 1
refdes=CONN?
T 56800 28950 5 10 0 0 180 0 1
device=CONNECTOR_8
T 56800 28750 5 10 0 0 180 0 1
footprint=SIP8N
}
C 65500 35400 1 0 0 resistor-1.sym
{
T 65800 35800 5 10 0 0 0 0 1
device=RESISTOR
T 65700 35700 5 10 1 1 0 0 1
refdes=R?
T 66000 35700 5 10 1 1 0 0 1
value=1k
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
N 69000 28400 69300 28400 4
{
T 69400 28400 5 10 1 1 0 0 1
netname=ENC_RED
}
N 69000 28000 69300 28000 4
{
T 69400 28000 5 10 1 1 0 0 1
netname=ENC_GRN
}
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
N 51100 31500 51800 31500 4
{
T 51900 31500 5 10 1 1 0 0 1
netname=ENC_C
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
netname=I2C2_SCL
}
N 75600 30000 76300 30000 4
{
T 76400 30000 5 10 1 1 0 0 1
netname=SPI1_CS0
}
N 75600 29600 76300 29600 4
{
T 76400 29600 5 10 1 1 0 0 1
netname=SPI1_MISO
}
C 55100 29700 1 0 0 resistor-1.sym
{
T 55400 30100 5 10 0 0 0 0 1
device=RESISTOR
T 55300 29900 5 10 1 1 0 0 1
refdes=R?
T 55600 29900 5 10 1 1 0 0 1
value=1k
}
C 55100 29600 1 90 0 3.3V-plus-1.sym
N 56000 29800 57600 29800 4
C 55100 30100 1 0 0 resistor-1.sym
{
T 55400 30500 5 10 0 0 0 0 1
device=RESISTOR
T 55300 30300 5 10 1 1 0 0 1
refdes=R?
T 55600 30300 5 10 1 1 0 0 1
value=1k
}
C 55100 30000 1 90 0 3.3V-plus-1.sym
N 56000 30200 57600 30200 4
C 55100 30500 1 0 0 resistor-1.sym
{
T 55400 30900 5 10 0 0 0 0 1
device=RESISTOR
T 55300 30700 5 10 1 1 0 0 1
refdes=R?
T 55600 30700 5 10 1 1 0 0 1
value=1k
}
C 55100 30400 1 90 0 3.3V-plus-1.sym
N 56000 30600 57600 30600 4
C 55100 30900 1 0 0 resistor-1.sym
{
T 55400 31300 5 10 0 0 0 0 1
device=RESISTOR
T 55300 31100 5 10 1 1 0 0 1
refdes=R?
T 55600 31100 5 10 1 1 0 0 1
value=1k
}
C 55100 30800 1 90 0 3.3V-plus-1.sym
N 56000 31000 57600 31000 4
C 55100 31300 1 0 0 resistor-1.sym
{
T 55400 31700 5 10 0 0 0 0 1
device=RESISTOR
T 55300 31500 5 10 1 1 0 0 1
refdes=R?
T 55600 31500 5 10 1 1 0 0 1
value=1k
}
C 55100 31200 1 90 0 3.3V-plus-1.sym
N 56000 31400 57600 31400 4
C 55100 31700 1 0 0 resistor-1.sym
{
T 55400 32100 5 10 0 0 0 0 1
device=RESISTOR
T 55300 31900 5 10 1 1 0 0 1
refdes=R?
T 55600 31900 5 10 1 1 0 0 1
value=1k
}
C 55100 31600 1 90 0 3.3V-plus-1.sym
N 56000 31800 57600 31800 4
C 55100 32100 1 0 0 resistor-1.sym
{
T 55400 32500 5 10 0 0 0 0 1
device=RESISTOR
T 55300 32300 5 10 1 1 0 0 1
refdes=R?
T 55600 32300 5 10 1 1 0 0 1
value=1k
}
C 55100 32000 1 90 0 3.3V-plus-1.sym
N 56000 32200 57600 32200 4
C 48400 21600 1 0 0 lp3982.sym
{
T 50200 24100 5 10 1 1 0 6 1
refdes=U?
T 48800 24300 5 10 0 0 0 0 1
device=LP3982
T 48800 24500 5 10 0 0 0 0 1
footprint=TSSOP-65P-640L1-14N
}
C 48500 23400 1 90 0 5V-plus-1.sym
C 47600 23000 1 0 0 capacitor-1.sym
{
T 47800 23700 5 10 0 0 0 0 1
device=CAPACITOR
T 47700 23300 5 10 1 1 0 0 1
refdes=C?
T 47800 23900 5 10 0 0 0 0 1
symversion=0.1
T 47600 23000 5 10 1 1 0 0 1
value=33n
}
C 47300 23300 1 270 0 gnd-1.sym
C 49400 21400 1 0 0 gnd-1.sym
N 48500 22800 47900 22800 4
{
T 47800 22800 5 10 1 1 0 6 1
netname=XBEE_PWR
}
N 50500 23200 50500 23600 4
N 50500 23600 53200 23600 4
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
C 58500 39500 1 0 0 switch-pushbutton-no-1.sym
{
T 58900 39800 5 10 1 1 0 0 1
refdes=BTN1
T 58900 40100 5 10 0 0 0 0 1
device=SWITCH_PUSHBUTTON_NO
}
C 58500 38800 1 0 0 switch-pushbutton-no-1.sym
{
T 58900 39100 5 10 1 1 0 0 1
refdes=BTN2
T 58900 39400 5 10 0 0 0 0 1
device=SWITCH_PUSHBUTTON_NO
}
C 58500 38100 1 0 0 switch-pushbutton-no-1.sym
{
T 58900 38400 5 10 1 1 0 0 1
refdes=BTN3
T 58900 38700 5 10 0 0 0 0 1
device=SWITCH_PUSHBUTTON_NO
}
C 58500 37400 1 0 0 switch-pushbutton-no-1.sym
{
T 58900 37700 5 10 1 1 0 0 1
refdes=BTN4
T 58900 38000 5 10 0 0 0 0 1
device=SWITCH_PUSHBUTTON_NO
}
C 60400 39700 1 270 0 3.3V-plus-1.sym
C 59500 39400 1 0 0 resistor-1.sym
{
T 59800 39800 5 10 0 0 0 0 1
device=RESISTOR
T 59700 39600 5 10 1 1 0 0 1
refdes=R?
T 60000 39600 5 10 1 1 0 0 1
value=1k
}
C 60400 39000 1 270 0 3.3V-plus-1.sym
C 59500 38700 1 0 0 resistor-1.sym
{
T 59800 39100 5 10 0 0 0 0 1
device=RESISTOR
T 59700 38900 5 10 1 1 0 0 1
refdes=R?
T 60000 38900 5 10 1 1 0 0 1
value=1k
}
C 60400 38300 1 270 0 3.3V-plus-1.sym
C 59500 38000 1 0 0 resistor-1.sym
{
T 59800 38400 5 10 0 0 0 0 1
device=RESISTOR
T 59700 38200 5 10 1 1 0 0 1
refdes=R?
T 60000 38200 5 10 1 1 0 0 1
value=1k
}
C 60400 37600 1 270 0 3.3V-plus-1.sym
C 59500 37300 1 0 0 resistor-1.sym
{
T 59800 37700 5 10 0 0 0 0 1
device=RESISTOR
T 59700 37500 5 10 1 1 0 0 1
refdes=R?
T 60000 37500 5 10 1 1 0 0 1
value=1k
}
N 58500 39500 57900 39500 4
{
T 57800 39500 5 10 1 1 0 6 1
netname=BTN1
}
N 58500 38800 57900 38800 4
{
T 57800 38800 5 10 1 1 0 6 1
netname=BTN2
}
N 58500 38100 57900 38100 4
{
T 57800 38100 5 10 1 1 0 6 1
netname=BTN3
}
N 58500 37400 57900 37400 4
{
T 57800 37400 5 10 1 1 0 6 1
netname=BTN4
}
N 73000 30400 72300 30400 4
{
T 72200 30400 5 10 1 1 0 6 1
netname=BTN3
}
N 73000 30800 72300 30800 4
{
T 72200 30800 5 10 1 1 0 6 1
netname=BTN2
}
N 73000 30000 72300 30000 4
{
T 72200 30000 5 10 1 1 0 6 1
netname=BTN4
}
N 73000 32400 72300 32400 4
{
T 72200 32400 5 10 1 1 0 6 1
netname=BTN1
}
C 69000 35400 1 270 0 connector2-2.sym
{
T 70300 34700 5 10 1 1 270 6 1
refdes=CONN?
T 70250 35100 5 10 0 0 270 0 1
device=CONNECTOR_2
T 70450 35100 5 10 0 0 270 0 1
footprint=SIP2N
}
N 69400 35400 69400 36000 4
N 69800 35300 69800 36000 4
N 69800 36000 69900 36000 4
C 61100 34300 1 90 0 resistor-1.sym
{
T 60700 34600 5 10 0 0 90 0 1
device=RESISTOR
T 60900 34500 5 10 1 1 90 0 1
refdes=R?
T 60900 34800 5 10 1 1 90 0 1
value=1k
}
N 61000 34300 61000 33700 4
{
T 61000 33600 5 10 1 1 90 6 1
netname=ADC_MISO
}
C 60800 35200 1 0 0 3.3V-plus-1.sym
C 61700 34300 1 90 0 resistor-1.sym
{
T 61300 34600 5 10 0 0 90 0 1
device=RESISTOR
T 61500 34500 5 10 1 1 90 0 1
refdes=R?
T 61500 34800 5 10 1 1 90 0 1
value=1k
}
N 61600 34300 61600 33700 4
{
T 61600 33600 5 10 1 1 90 6 1
netname=ADC_MOSI
}
C 61400 35200 1 0 0 3.3V-plus-1.sym
C 62300 34300 1 90 0 resistor-1.sym
{
T 61900 34600 5 10 0 0 90 0 1
device=RESISTOR
T 62100 34500 5 10 1 1 90 0 1
refdes=R?
T 62100 34800 5 10 1 1 90 0 1
value=1k
}
C 62000 35200 1 0 0 3.3V-plus-1.sym
N 62200 34300 62200 33700 4
{
T 62200 33600 5 10 1 1 90 6 1
netname=ADC_SCLK
}
