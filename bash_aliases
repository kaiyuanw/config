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
ssh_username_default="pynie"
ssh_options_default=""

declare -A ssh_ip=(
        ["xps1"]="10.157.90.131" # "xps1.ece.utexas.edu"
        ["xps2"]="10.157.90.132" # "xps2.ece.utexas.edu"
        ["istanbul"]="10.157.90.137" # "istanbul.ece.utexas.edu"
        ["cozy"]="cozy.ece.utexas.edu"
)
declare -A ssh_port=(
        ["xps1"]="1453"
        ["xps2"]="1453"
        ["istanbul"]="1453"
        ["cozy"]="2002"
)

function cs() {
        local funcname=${FUNCNAME[0]}
        function usage() {
                printf "usage: ${funcname} [target] (-u [username] -X)\n"
        }
        function error() {
                printf "error: ${1}\n" 1>&2
                usage 1>&2
        }
        
        if [[ $# -lt 1 ]]; then error "no arguments supplied"; return; fi
        local to="${1}"; shift

        local ip="${ssh_ip[$to]}"
        if [[ -z "$ip" ]]; then
                ip="$to"
        fi
        local port="${ssh_port[$to]}"

        # parse options
        local username="${ssh_username_default}"
        local options="${ssh_options_default}"
        while [[ $# -ge 1 ]]; do
                case ${1} in
                -h|-\?|--help)
                        usage; return
                        ;;
                -u|--user)
                        if [[ $# -ge 2 ]]; then
                                username="${2}"
                                shift
                        else
                                error "no username supplied"; return
                        fi
                        ;;
                -X)
                        options="${options} -X"
                        ;;
                -p|--port)
                        if [[ $# -ge 2 ]]; then
                                port="${2}"
                                shift
                        else
                                error "no port supplied"; return
                        fi
                        ;;
                esac
        done

        local options_port=""
        if [[ ! -z "${port}" ]]; then
                options_port="-p ${port}"
        fi
        options="${options} ${options_port}"

        ssh -A ${options} "${username}@${ip}"
}

alias xps1="cs xps1"
alias xps2="cs xps2"
alias istanbul="cs istanbul"
alias cozy="cs cozy"

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
alias g='git status -s; git status -s | rev | cut -d " " -f 1 | rev; git status -s | rev | cut -d " " -f 1 | rev | xargs echo'
alias gg='git fetch && git rebase && git push'

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
