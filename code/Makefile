all : heater-bone.dtbo

%.dtbo : %.dts
	dtc $<  -@ -O dtb -o $@ -b0

.PHONY : install-dtb
install-dtb : heater-bone.dtbo
	modprobe pwm-pca9685
	cp $< /lib/firmware/heater-bone-00A0.dtbo

load-dtb : install-dtb
	echo heater-bone > /sys/devices/bone_capemgr.6/slots

export-gpios :
	echo 44 > /sys/class/gpio/export
	echo out > /sys/class/gpio/gpio44/direction
	echo 45 > /sys/class/gpio/export
	echo out > /sys/class/gpio/gpio45/direction

