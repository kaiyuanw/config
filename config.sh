#!/bin/bash

_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# supported configurations
ALL_CONFIGS=".bashrc .profile .emacs .gitconfig .hgrc"

# install configurations by including these config files to system config files

function configure() {
        local target_file="${1}"; shift
        local source_file="${1}"; shift
        local detect_line="$(sed -n '1p' ${source_file})"; if [[ $# -ge 1 ]]; then detect_line="${1}"; shift; fi

        local DIR_escaped=${_DIR//\//\\\/}

        if [[ -f ${target_file} ]]; then
                if [[ -z $(grep "${detect_line}" "${target_file}") ]]; then
                        sed "s/###DIR###/${DIR_escaped}/g" "${source_file}" >> "${target_file}"
                        printf "> CONFIGURED ${target_file}\n"
                else
                        printf "+ SKIPPED ${target_file}\n"
                fi
        else
                printf "x NOT FOUND ${target_file}\n"
        fi
}

# revert configurations by removing the link from system config files

function revert() {
        local target_file="${1}"; shift
        local source_file="${1}"; shift
        local detect_line_beg="$(sed -n '1p' ${source_file})"; if [[ $# -ge 1 ]]; then detect_line_beg="${1}"; shift; fi
        local detect_line_end="$(awk '/./{line=$0} END{print line}' ${source_file})"; if [[ $# -ge 1 ]]; then detect_line_end="${1}"; shift; fi

        
        if [[ -f ${target_file} ]]; then
                local beg=$(grep "${detect_line_beg}" "${target_file}" -n | cut -d ":" -f 1)
                if [[ -z ${beg} ]]; then
                        printf "+ SKIPPED ${target_file}\n"
                else
                        local end=$(grep "${detect_line_end}" "${target_file}" -n | cut -d ":" -f 1)
                        sed -i "$beg,$end d" "${target_file}"
                        printf "> REVERTED ${target_file}\n"
                fi
        else
                printf "x NOT FOUND ${target_file}\n"
        fi
}

function main() {
        local operation="config"; if [[ $# -ge 1 ]]; then operation="${1}"; shift; fi
        local configs=${ALL_CONFIGS}; if [[ $# -ge 1 ]]; then configs=$@; fi

        if [[ ${operation} == "config" ]]; then
                for config in ${configs}; do
                        configure "$HOME/${config}" "$_DIR/HOME/${config}"
                done
        elif [[ ${operation} == "revert" ]]; then
                for config in ${configs}; do
                        revert "$HOME/${config}" "$_DIR/HOME/${config}"
                done
        else
                printf "Unrecognized option\n"
                printf "Usage: "$(basename "$0")" [config/revert]\n"
        fi
}

main $@
