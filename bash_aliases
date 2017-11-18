#!/bin/bash

# workspace
alias s='screen -e"^Zz" -D -R'
alias e='emacs -nw'
alias emacskill="emacsclient -e '(kill-emacs)'"

# ssh
function ssh-refresh() {
        eval $(ssh-agent)
        ssh-add
}

# UT Servers
#xps1_ip="10.157.90.131"
xps1_ip="xps1.ece.utexas.edu"
alias xps1="ssh -A -p 1453 pynie@${xps1_ip}"
alias xps1_X="ssh -A -p 1453 pynie@${xps1_ip} -X"
#xps2_ip="10.157.90.132"
xps2_ip="xps2.ece.utexas.edu"
alias xps2="ssh -A -p 1453 pynie@${xps2_ip}"
alias xps2_X="ssh -A -p 1453 pynie@${xps2_ip} -X"
#istanbul_ip="10.157.90.137"
istanbul_ip="istanbul.ece.utexas.edu"
alias istanbul="ssh -A -p 1453 pynie@${istanbul_ip}"
alias istanbul_X="ssh -A -p 1453 pynie@${istanbul_ip} -X"
alias cozy='ssh -A -p 2002 pynie@cozy.ece.utexas.edu'
alias cozy_X='ssh -A -p 2002 pynie@cozy.ece.utexas.edu -X'

function hgclonework() {
        local name="${1}"; shift
        local option=$@
        hg clone ssh://work@cozy.ece.utexas.edu:2002//home/work/projects/${name} "$@"
}

function clonework() {
        local name="${1}"; shift
        local option=$@
        git clone ssh://work@cozy.ece.utexas.edu:2002//home/work/projects/${name} "$@"
}

function clonecozy() {
        local name="${1}"; shift
        local option=$@
        git clone ssh://pynie@cozy.ece.utexas.edu:2002//home/pynie/projects/${name} "$@"
}

function clonerepo() {
        local name="${1}"; shift
        local option=$@
        git clone ssh://pynie@cozy.ece.utexas.edu:2002//home/repos/projects/${name} "$@"
}

# # proxy
alias proxy_xps1="ssh -C2qTnN -p 1453 -D 8080 pynie@${xps1_ip}"
alias setproxy_xps1='export http_proxy=socks5://localhost:8080; export https_proxy=socks5://localhost:8080; export all_proxy=socks5://localhost:8080'
alias proxy_xps2="ssh -C2qTnN -p 1453 -D 8080 pynie@${xps2_ip}"
alias setproxy_xps2='export http_proxy=socks5://localhost:8080; export https_proxy=socks5://localhost:8080; export all_proxy=socks5://localhost:8080'
alias proxy_cozy='ssh -C2qTnN -p 2002 -D 8080 pynie@cozy.ece.utexas.edu'
alias setproxy_cozy='export http_proxy=socks5://localhost:8080; export https_proxy=socks5://localhost:8080; export all_proxy=socks5://localhost:8080'
alias unsetproxy='unset http_proxy; unset https_proxy; unset all_proxy'

# python
alias py3='source ~/opt/py3/bin/activate'
alias py2='source ~/opt/py2/bin/activate'

# google cloud
alias gcloud_test='${HOME}/opt/google-cloud-sdk/bin/dev_appserver.py'

# google chrome
alias web='google-chrome'

# git
alias g='git status -s'

# make
alias cleanmake='make clean && make'

# spoon
alias spoon='java -cp $HOME/opt/spoon.jar'

# antlr4
export CLASSPATH="$HOME/opt/antlr-runtime-4.7.jar:${CLASSPATH}"
alias antlr4='java -Xmx500M -cp "$HOME/opt/antlr-runtime-4.7.jar:$CLASSPATH" org.antlr.v4.Tool'
alias grun='java org.antlr.v4.gui.TestRig'

# cuda
export PATH="/usr/local/cuda-9.0/bin:$PATH"

# maven
function mvn_init() {
        local groupId="${1}"; shift
        local artifactId="${1}"; shift

        mvn -B archetype:generate \
            -DarchetypeGroupId=org.apache.maven.archetypes \
            -DgroupId="${groupId}" \
            -DartifactId="${artifactId}"
}
