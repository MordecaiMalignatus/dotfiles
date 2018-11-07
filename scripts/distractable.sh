#!/usr/bin/bash

set -e

echo "Blocking distractions..."
sudo -v

sudo sed -i "$ a 0.0.0.0 hckrnews.com" /etc/hosts
sudo sed -i "$ a 0.0.0.0 news.ycombinator.com" /etc/hosts
sudo sed -i "$ a 0.0.0.0 www.reddit.com" /etc/hosts
sudo sed -i "$ a 0.0.0.0 www.nytimes.com" /etc/hosts
sudo sed -i "$ a 0.0.0.0 lobste.rs" /etc/hosts
