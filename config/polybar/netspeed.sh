#!/bin/bash

R1=`cat /sys/class/net/enp3s0/statistics/rx_bytes`
T1=`cat /sys/class/net/enp3s0/statistics/tx_bytes`
sleep 1
R2=`cat /sys/class/net/enp3s0/statistics/rx_bytes`
T2=`cat /sys/class/net/enp3s0/statistics/tx_bytes`
TBPS=`expr $T2 - $T1`
RBPS=`expr $R2 - $R1`
TKBPS=`expr $TBPS / 1024`
RKBPS=`expr $RBPS / 1024`
echo "⬇ $RKBPS kB/s ⬆ $TKBPS kB/s"