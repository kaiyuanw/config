#!/bin/bash

# shared .bashrc

_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# machine specific behaviour
if [[ -f ${_DIR}/machine.sh ]]; then
        . ${_DIR}/machine.sh
fi

# user local program
export PATH="$HOME/bin:$PATH"

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

## Java 9
export JAVA9_VERSION="9.0.1"
export JAVA9_HOME="${HOME}/opt/jdk-${JAVA9_VERSION}"

## Java 8
export JAVA8_VERSION="162"
export JAVA8_HOME="${HOME}/opt/jdk1.8.0_${JAVA8_VERSION}"

## Java 7 @Deprecated
export JAVA7_VERSION="80"
export JAVA7_HOME="${HOME}/opt/jdk1.7.0_${JAVA7_VERSION}"

export JAVA_HOME=${JAVA8_HOME}
export CLASSPATH="."

# xps2 Java
if [[ "${MACHINE}" == "xps2" ]]; then
        export JAVA7_HOME="/home/celik/opt/jdk1.7.0_72"
        export JAVA8_HOME="/home/celik/opt/jdk1.8.0_60"
        export JAVA_HOME="${JAVA8_HOME}"
fi

export PATH="$JAVA_HOME/bin:$PATH"

export MAVEN_HOME="${HOME}/opt/apache-maven-3.5.2"
export PATH="${MAVEN_HOME}/bin:$PATH"

export ANT_HOME="${HOME}/opt/apache-ant-1.9.3"
export PATH="${ANT_HOME}/bin:$PATH"
