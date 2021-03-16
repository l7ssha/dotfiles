#!/bin/sh

case "$1" in
"genconf")
    docker run -v $OVPN_DATA:/etc/openvpn --rm kylemanna/openvpn ovpn_genconfig -u udp://l7ssha.xyz
    ;;
"initconf")
    docker run -v $OVPN_DATA:/etc/openvpn --rm -it kylemanna/openvpn ovpn_initpki
    ;;
"startserver")
    docker run -v $OVPN_DATA:/etc/openvpn -d -p 1194:1194/udp --name openvpn --cap-add=NET_ADMIN kylemanna/openvpn
    ;;
"genconfig")
    docker run -v $OVPN_DATA:/etc/openvpn --rm -it kylemanna/openvpn easyrsa build-client-full "$2" nopass
    docker run -v $OVPN_DATA:/etc/openvpn --rm kylemanna/openvpn ovpn_getclient "$2" > "$2".ovpn
    ;;
esac
