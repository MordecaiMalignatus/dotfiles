#!/usr/bin/bash

set -e

echo "Wait a bit, would you?"
sudo -v

sleep 10

echo "Unlocking timewasters..."
sudo sed -i "s/^0\.0.*//g" /etc/hosts
sudo sed -i "/^\s*$/d" /etc/hosts
