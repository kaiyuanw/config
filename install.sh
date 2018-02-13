#!/bin/bash

tar xvf $HOME/opt/*.tar.gz
# rm $HOME/opt/*.tar.gz
ssh-keygen -t rsa -C "wangkaiyuanzz@gmail.com"

sudo apt-get install xvfb
sudo apt-get install xorg
sudo apt-get install gtk2.0
