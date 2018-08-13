#!/bin/sh
date=`date +%s`

maim -s ~/.screenshots/$date.png
xclip -selection clipboard -t image/png < ~/.screenshots/$date.png
