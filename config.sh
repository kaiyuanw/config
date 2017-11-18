#!/bin/bash

_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# install these configurations by including this path to system default config files

function configure() {
        local target_file="${1}"; shift
        local source_file="${1}"; shift
        local detect_line="${1}"; shift
        if [[ -f ${target_file} ]]; then
                if [[ -z $(grep "${detect_line}" "${target_file}") ]]; then
                        cat "${source_file}" >> "${target_file}"
                        printf "> CONFIGURED ${target_file}\n"
                else
                        printf "+ SKIPPED ${target_file}\n"
                fi
        else
                printf "x NOT FOUND ${target_file}\n"
        fi
}

# bashrc
configure "$HOME/.bashrc" "$_DIR/HOME/.bashrc" "########## Auto Generated by cfg ##########"
# profile
configure "$HOME/.profile" "$_DIR/HOME/.profile" "########## Auto Generated by cfg ##########"
# emacs
configure "$HOME/.emacs" "$_DIR/HOME/.emacs" ";;;;;;;;;; Auto Generated by cfg ;;;;;;;;;;"
# gitconfig
configure "$HOME/.gitconfig" "$_DIR/HOME/.gitconfig" "########## Auto Generated by cfg ##########"
# hgrc
configure "$HOME/.hgrc" "$_DIR/HOME/.hgrc" "########## Auto Generated by cfg ##########"
