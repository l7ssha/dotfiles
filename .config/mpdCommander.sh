#!/usr/bin/env bash

SCRIPTPATH="$(cd "$(dirname "$0")" ; pwd -P)"
mpc="mpc --host $1 --port $2"

playing=$($mpc status -f '%artist% - %title%' | sed -n 1p)

if [[ $playing == volume* ]]; then
    playing=""
fi

choice=$(cat $SCRIPTPATH"/commands.txt" | dmenu -i -p "$playing")

echo $SCRIPTPATH"/commands.txt"

if [[ $choice == play* ]] || [[ $choice == volume* ]]; then
    $mpc $choice >> /dev/null
    exit 0
fi

case $choice in
    "toggle")
        $mpc toggle
        ;;
    "stop")
        $mpc stop
        ;;
    "next")
        $mpc next
        ;;
    "prev")
        $mpc prev
        ;;
    *)
        exit 0
        ;;
esac
