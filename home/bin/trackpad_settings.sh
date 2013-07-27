# Apple Magic Trackpad

if [ xinput list 'Apple Magic Trackpad' &> /dev/null ]; then
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

    xset m 4 1
fi

# Apple Magic Mouse
if [ xinput list 'Apple Magic Mouse' &> /dev/null ]; then 
    xinput set-prop 'Apple Magic Mouse' 'Device Accel Constant Deceleration' 1
    xinput set-prop 'Apple Magic Mouse' 'Device Accel Adaptive Deceleration' 1
    xinput set-prop 'Apple Magic Mouse' 'Device Accel Velocity Scaling' 1
    xset m 1 1
fi

# Trackpoint
if xinput list 'TPPS/2 IBM TrackPoint' &> /dev/null; then
    xset m 4 0
fi
