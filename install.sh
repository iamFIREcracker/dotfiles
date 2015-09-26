#!/bin/bash

set -u



do_echo()
{
    local kind=$1
    local target="${2}"

    echo "${kind}    ${target}"
}


do_install()
{
    local src="$1"
    local dst="$2"
    local force="$3"

    if [ -d "${src}" ]; then
        if [ -d "${dst}" ]; then
            continue
        else
            if [ -e "${dst}" ]; then
                rm -rf "${dst}"
                do_echo D "${dst}"
            fi
            mkdir "${dst}"
            do_echo A "${dst}"
        fi
    else
        if [ -h "${dst}" -a $force -eq 0 ]; then
            continue
        else
            if [ -f "${dst}" ]; then
                rm -rf "${dst}"
                do_echo D "${dst}"
            fi
            ln -s "${src}" "${dst}"
            do_echo L "${dst}"
        fi
    fi
}


do_process()
{
    local current="$1"
    local sources="$2"
    local force="$3"

    for path in `find -X -L ${sources} ! -path '*.svn*' ! -name '*.un~'`; do
        if [ ${path} == ${sources} ]; then
            continue
        fi

        src="${current}/${path}"
        dst="$HOME/${path#${sources}}"
        do_install "${src}" "${dst}" "$force"
    done
}



# parse the sources directory
if [ $# -lt 1 ]; then
    echo "Usage: $0 <sources> [-f]"
    exit 1
fi
sources="$1"

# check if optional -f(orce) flag is set
force=0
if [ $# -ge 2 ]; then
    if [ "$2" == "-f" ]; then
        force=1
    fi
fi

# check if the sources directory is either relative or absolute and fill
# the variable current so that ${current}${sources} return an absolute
# link.
if [ "${sources:0:1}" != '/' ]; then
    current="`pwd`"
fi

do_process "${current}" "${sources}" "$force"
