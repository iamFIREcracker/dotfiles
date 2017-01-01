#!/usr/bin/env bash


# Kernel source tree
KERNELTREE=~/workspace/FreeBSD/head

# Netmap source tree: development and release
NETMAPDEV=~/workspace/netmap/v2
NETMAPREL=~/workspace/netmap/release

# Netmap core files
NETMAPKOR="$(find ${NETMAPDEV}/sys -type f)"

# Modified drivers
PATCHED='
        sys/dev/ixgbe/ixgbe.c
        sys/dev/e1000/if_lem.c
        sys/dev/e1000/if_em.c
        sys/dev/e1000/if_igb.c
        sys/dev/re/if_re.c
        sys/dev/bge/if_bge.c
        sys/net/if_var.h
	sys/conf/files
        release/picobsd/build/picobsd
        release/picobsd/floppy.tree/etc/rc1
        release/picobsd/floppy.tree/etc/hosts
'

# Install new files only if they differ in content with targets
function do_copy
{
    diff -q $1 $2 2>&1 > /dev/null && return

    echo $2
    mkdir -p $(dirname $2)
    cp $1 $2
}

# Install dev files from ${NETMAPDEV} in ${KERNELTREE}
function ninstall
{
    local f

    # copy netmap core files
    for f in ${NETMAPKOR}; do
        f=${f##${NETMAPDEV}/}
        do_copy ${NETMAPDEV}/${f} ${KERNELTREE}/${f}
    done

    # and now patched drivers
    for f in ${PATCHED}; do
        do_copy ${NETMAPDEV}/patched/$(basename ${f}) ${KERNELTREE}/${f}
    done

    echo missing netmap headers links inside /usr/include/net # XXX
}

# Revert kernel source tree
function nuninstall
{
    local dst

    svn revert -R ${KERNELTREE}/sys
}

# Import changes from ${KERNELTREE} to ${NETMAPDEV}
function nimport
{
    local f

    # import netmap core files
    for f in ${NETMAPKOR}; do
        f=${f##${NETMAPDEV}/}
        do_copy ${KERNELTREE}/${f} ${NETMAPDEV}/${f}
    done

    # and drivers
    for f in ${PATCHED}; do
        do_copy ${KERNELTREE}/${f} ${NETMAPDEV}/patched/$(basename ${f})
    done

    # for backup, the kernel diff
    (
    cd ${KERNELTREE}/sys
    svn diff > ${NETMAPDEV}/head-netmap.diff
    )

    # picobsd changes
    (
    cd ${KERNELTREE}/release/picobsd
    svn diff > ${NETMAPDEV}/picobsd.diff
    )
}

function do_merge
{
    local src=$1
    local dst=$2

    if [ -f ${dst} ]; then
        read -p "Merge ${dst}? [N/y] "
        [ "${REPLY}" = 'y' ] && vimdiff ${dst} ${src}
    else
        read -p "Import ${src}? [N/y] "
        [ "${REPLY}" = 'y' ] && (
            cp -R ${src} ${dst}
            svn add ${dst}
        )
    fi
}

# Merge changes coming from ${NETMAPREL} into ${NETMAPDEV}
# It is possible to filter file to merge passing a regexp as first argument.
function nmerge
{
    local line src dst
    local regexp=${1:-.*}
    
    diff -urq ${NETMAPREL} ${NETMAPDEV} | \
    grep -v '.svn' | \
    while read line; do
        set -- ${line}
        if [[ "${line}" =~ 'Only in' ]]; then
            src=${3%%:}/$4
            if [[ ${src} =~ ${NETMAPREL} ]]; then
                dst=${NETMAPDEV}/${src##${NETMAPREL}/}
                [[ "${src}" =~ ${regexp} ]] && do_merge ${src} ${dst} < /dev/tty
            fi
        elif [[ "${line}" =~ 'Files ' ]]; then
            src=$2
            dst=$4
            [[ "${src}" =~ ${regexp} ]] && do_merge ${src} ${dst} < /dev/tty
        else
            echo 'Aw shit!'
        fi
    done
}


# entry point
case "$1" in
install|uninstall|import)
    n$1
    ;;

merge)
    shift
    nmerge "$@"
    ;;

*)
    echo 'WTF?!'
esac
