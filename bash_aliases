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
        ["xps1"]="xps1.ece.utexas.edu" # "10.157.89.54"
        ["xps2"]="xps2.ece.utexas.edu" # "10.157.89.55"
        ["istanbul"]="istanbul.ece.utexas.edu" # "10.157.90.137"
        ["cozy"]="cozy.ece.utexas.edu"
)
declare -A ssh_port=(
        ["xps1"]="1453"
        ["xps2"]="1453"
        ["istanbul"]="1453"
        ["cozy"]="2002"
)

# connect ssh

function cs() {
        local funcname=${FUNCNAME[0]}
        function usage() {
                printf "usage: ${funcname} [ip] (-u [username] -X -p [port])\n"
                printf "registered ip names: ${!ssh_ip[*]}\n"
        }
        function error() {
                printf "error: ${1}\n" 1>&2
                usage 1>&2
        }
        
        # parse options
        local username="${ssh_username_default}"
        local port=""
        local options="${ssh_options_default}"
        local args=()
        while [[ $# -ge 1 ]]; do
                case ${1} in
                -h|-\?|--help)
                        usage; return
                        ;;
                -p|--port)
                        if [[ $# -ge 2 ]]; then
                                port="${2}"
                                shift
                        else
                                error "no port supplied"; return
                        fi
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

                --)
                        # no more arguments
                        shift; break
                        ;;
                -*)
                        error "unrecognized argument ${1}"; return
                        ;;
                *)
                        args+=(${1})
                esac
                shift
        done

        if [[ ${#args[@]} -lt 1 ]]; then error "no arguments supplied"; return; fi
        local to="${args[0]}"

        local ip="${ssh_ip[$to]}"
        if [[ -z "$ip" ]]; then
                ip="$to"
        fi
        if [[ -z "$port" ]]; then
                local port="${ssh_port[$to]}"
        fi

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

# connect ssh copy

function cscp() {
        local funcname=${FUNCNAME[0]}
        function usage() {
                printf "usage: ${funcname} [ip] [path] (-uP) - [ip] [path] (-up)\n"
                printf "       %${#funcname}s ^~~source~~~~~~~~   ^~~target~~~~~~~~\n" ""
                printf "registered ip names: local ${!ssh_ip[*]}\n"
        }
        function error() {
                printf "error: ${1}\n" 1>&2
                usage 1>&2
        }

        local options_global=""
        
        # parse source
        local username_src="${ssh_username_default}"
        local port_src=""
        local options_src="${ssh_options_default}"
        local args_src=()
        while [[ $# -ge 1 ]]; do
                case ${1} in
                -h|-\?|--help)
                        usage; return
                        ;;
                -P|--port)
                        if [[ $# -ge 2 ]]; then
                                port_src="${2}"
                                shift
                        else
                                error "no port supplied"; return
                        fi
                        ;;
                -u|--user)
                        if [[ $# -ge 2 ]]; then
                                username_src="${2}"
                                shift
                        else
                                error "no username supplied"; return
                        fi
                        ;;
                -X)
                        options_src="${options} -X"
                        ;;

                -)
                        # proceed to target options
                        shift; break
                        ;;
                --)
                        # no more arguments
                        shift; break
                        ;;
                -*)
                        # pass to scp
                        options_global="${options_global} ${1}"
                        ;;
                *)
                        args_src+=(${1})
                esac
                shift
        done

        if [[ ${#args_src[@]} -lt 1 ]]; then error "no arguments supplied"; return; fi
        local src="${args_src[0]}"
        local path_src=${args_src[@]:1}

        local pathprefix_src=""
        if [[ "$src" != "local" ]]; then
                local ip_src="${ssh_ip[$src]}"
                if [[ -z "$ip_src" ]]; then
                        ip_src="$src"
                fi
                pathprefix_src="${username_src}@${ip_src}:"
        fi

        if [[ -z "$port_src" ]]; then
                local port_src="${ssh_port[$src]}"
        fi
        local options_port_src=""
        if [[ ! -z "${port_src}" ]]; then
                options_port_src="-P ${port_src}"
        fi
        options_src="${options_src} ${options_port_src}"

        local scp_src="${options_src}"
        for p in ${path_src}; do
                scp_src="${scp_src} ${pathprefix_src}${p}"
        done

        # parse target
        local username_tgt="${ssh_username_default}"
        local port_tgt=""
        local options_tgt="${ssh_options_default}"
        local args_tgt=()
        while [[ $# -ge 1 ]]; do
                case ${1} in
                -h|-\?|--help)
                        usage; return
                        ;;
                -P|--port)
                        if [[ $# -ge 2 ]]; then
                                port_tgt="${2}"
                                shift
                        else
                                error "no port supplied"; return
                        fi
                        ;;
                -u|--user)
                        if [[ $# -ge 2 ]]; then
                                username_tgt="${2}"
                                shift
                        else
                                error "no username supplied"; return
                        fi
                        ;;
                -X)
                        options_tgt="${options} -X"
                        ;;

                --)
                        # no more arguments
                        shift; break
                        ;;
                -*)
                        # pass to scp
                        options_global="${options_global} ${1}"
                        ;;
                *)
                        args_tgt+=(${1})
                esac
                shift
        done

        if [[ ${#args_tgt[@]} -lt 1 ]]; then error "no arguments supplied"; return; fi
        local tgt="${args_tgt[0]}"
        local path_tgt=${args_tgt[@]:1}

        local pathprefix_tgt=""
        if [[ "$tgt" != "local" ]]; then
                local ip_tgt="${ssh_ip[$tgt]}"
                if [[ -z "$ip_tgt" ]]; then
                        ip_tgt="$tgt"
                fi
                pathprefix_tgt="${username_tgt}@${ip_tgt}:"
        fi
        
        if [[ -z "$port_tgt" ]]; then
                local port_tgt="${ssh_port[$tgt]}"
        fi

        local options_port_tgt=""
        if [[ ! -z "${port_tgt}" ]]; then
                options_port_tgt="-P ${port_tgt}"
        fi
        options_tgt="${options_tgt} ${options_port_tgt}"

        local scp_tgt="${options_tgt} ${pathprefix_tgt}${path_tgt}"

        # call scp
        # printf "scp ${options_global} ${scp_src} ${scp_tgt}\n"
        scp ${options_global} ${scp_src} ${scp_tgt}
}


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
function mvn-init() {
        local groupId="${1}"; shift
        local artifactId="${1}"; shift

        mvn -B archetype:generate \
            -DarchetypeGroupId=org.apache.maven.archetypes \
            -DgroupId="${groupId}" \
            -DartifactId="${artifactId}"
}

# onedrive

function onedrive-start() {
        systemctl --user enable onedrive
        systemctl --user start onedrive
}

function onedrive-log() {
        journalctl --user-unit onedrive -f
}
