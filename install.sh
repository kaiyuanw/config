#!/bin/bash

sudo apt-get update
sudo apt-get install emacs

mkdir $HOME/opt
mkdir $HOME/projects

(
        cd $HOME/opt
        tar xvf $HOME/opt/jdk-7u80-linux-x64.tar.gz
        tar xvf $HOME/opt/apache-maven-3.5.2-bin.tar.gz
        tar xvf $HOME/opt/apache-ant-1.9.3-bin.tar.gz
)

# rm $HOME/opt/*.tar.gz
ssh-keygen -t rsa -C "wangkaiyuanzz@gmail.com"

sudo apt-get install xvfb
sudo apt-get install xorg
sudo apt-get install gtk2.0
