#!/usr/bin/bash

set -e

echo "unblocking timewasters..."
sudo -v

sudo sed -i "s/^0\.0.*//g" /etc/hosts
sudo sed -i "/^\s*$/d" /etc/hosts
