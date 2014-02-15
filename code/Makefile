all : heater-bone.dtbo

%.dtbo : %.dts
	dtc $<  -@ -O dtb -o $@ -b0

.PHONY : install-dtb
install-dtb : heater-bone.dtbo
	modprobe pwm-pca9685
	cp $< /lib/firmware/heater-bone-00A0.dtbo

.PHONY : load-dtb
load-dtb : install-dtb
	echo heater-bone > /sys/devices/bone_capemgr.6/slots

.PHONY : export-gpios
export-gpios :
	echo 44 > /sys/class/gpio/export
	echo out > /sys/class/gpio/gpio44/direction
	echo 45 > /sys/class/gpio/export
	echo out > /sys/class/gpio/gpio45/direction

.PHONY : dump-pins
dump-pins :
	cat /sys/kernel/debug/pinctrl/*.pinmux/pins

.PHONY : dump-pinmux-pins
dump-pinmux-pins :
	cat /sys/kernel/debug/pinctrl/*.pinmux/pinmux-pins

.PHONY : dump-pin-groups
dump-pin-groups :
	cat /sys/kernel/debug/pinctrl/*.pinmux/pingroups
