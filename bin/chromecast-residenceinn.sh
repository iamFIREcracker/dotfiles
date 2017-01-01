#!/bin/env bash

set -e

IF_NAME='en0'
CHROMECAST_HWADDR='6c:ad:f8:5d:28:23'


original_if_address=`ifconfig ${IF_NAME} | awk '/ether/ { print $2 }'`

echo "Setting interface ($IF_NAME) mac address to $CHROMECAST_HWADDR"
sudo ifconfig $IF_NAME ether $CHROMECAST_HWADDR

echo "Shutting down interface ($IF_NAME)"
sudo ifconfig $IF_NAME down

echo "Re-activate interface ($IF_NAME)"
sudo ifconfig $IF_NAME up

read -p "Log into the network, and press any key to continue"

echo "Setting interface ($IF_NAME) mac address to $original_if_address"
sudo ifconfig $IF_NAME ether $original_if_address

echo "Shutting down interface ($IF_NAME)"
sudo ifconfig $IF_NAME down

echo "Re-activate interface ($IF_NAME)"
sudo ifconfig $IF_NAME up
