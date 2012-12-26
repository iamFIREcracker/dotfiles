# Enable tapping
synclient TapButton1=1 TapButton2=3 TapButton3=2

# Increase tap-n-drag timeout time
#synclient SingleTapTimeout=360 FastTaps=1

# Enable horizontal scrolling
synclient HorizTwoFingerScroll=1

# Speed
synclient MinSpeed=1 MaxSpeed=2.5 AccelFactor=0.5

## Pressure sensitivity
#synclient FingerHigh=50

synclient ResolutionDetect=1

# Natural scrolling
#synclient VertScrollDelta=-157
#synclient HorizScrollDelta=-250
id=`xinput list | grep 'Apple Wireless Trackpad' | sed 's/.*id=\([0-9]*\).*/\1/'`
xinput set-prop $id "Synaptics Scrolling Distance" -157 -250

# Make pointer go 8x for each traversed pixel
xset m 4 1
