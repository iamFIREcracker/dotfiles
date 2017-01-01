#twm &
setxkbmap -option ctrl:nocaps
#xrdb -load ~/.Xdefaults
#xmodmap ~/.Xmodmap
#xsetroot -solid '#6c71c4'
#xclock -digit -geometry 297x35+1-1 &
##xterm -geometry 80x35+600+0 &
##xterm -geometry 167x40-0-0 &
#exec xterm -geometry 80x66+0+0 -name login
[ -n "`which xautolock`" -a -n "`which xlock`" ] && xautolock -time 15 -locker xlock &
exec awesome
