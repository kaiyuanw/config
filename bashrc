#!/bin/bash

# shared .bashrc

_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# machine specific behaviour
if [[ -f ${_DIR}/machine.sh ]]; then
        . ${_DIR}/machine.sh
fi

# user local program
export PATH="$HOME/bin:$PATH"

# ssh agent & key
if [[ -z "${SSH_AUTH_SOCK}" ]]; then
        eval $(ssh-agent)
        ssh-add
fi

# shared aliases
if [[ -f ${_DIR}/bash_aliases ]]; then
        . ${_DIR}/bash_aliases
fi

# bashutil
if [[ -f ~/projects/bashutil/OBash.sh ]]; then
        . ~/projects/bashutil/OBash.sh
fi

# opt libary
export PATH="$HOME/opt:$PATH"
export PATH="$HOME/opt/bin:$PATH"
# xps2 opt library
if [[ "${MACHINE}" == "xps2" ]]; then
        export PATH="/home/celik/opt:$PATH"
        # maven
        export PATH="/home/celik/opt/apache-maven-3.3.3/bin:$PATH"
        # cloc
        export PATH="/home/celik/opt/cloc:$PATH"
fi
        
# Java
export JAVA8_VERSION="152"
export JAVA8_HOME="${HOME}/opt/jdk1.8.0_${JAVA8_VERSION}"
export JAVA_HOME=${JAVA8_HOME}
export CLASSPATH="."
# xps2 Java
if [[ "${MACHINE}" == "xps2" ]]; then
        export JAVA7_HOME="/home/celik/opt/jdk1.7.0_72"
        export JAVA8_HOME="/home/celik/opt/jdk1.8.0_60"
        export JAVA_HOME="${JAVA8_HOME}"
fi

export PATH="$JAVA_HOME/bin:$PATH"

# environment
## editor
export EDITOR="emacsclient -n"

## history
# Undocumented feature which sets the size to "unlimited".
# http://stackoverflow.com/questions/9457233/unlimited-bash-history
export HISTFILESIZE=
export HISTSIZE=
export HISTTIMEFORMAT="[%F %T] "
# Change the file location because certain bash sessions truncate .bash_history file upon close.
# http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
export HISTFILE=~/.bash_eternal_history
# Force prompt to write history after every command.
# http://superuser.com/questions/20900/bash-history-loss
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

## screen
export TERM=xterm-256color

# Android Studio
export ANDROID_EMULATOR_USE_SYSTEM_LIBS=1
