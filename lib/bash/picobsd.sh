#!/usr/bin/env bash


# Kernel source tree
KERNELTREE=~/workspace/FreeBSD/head

# Netmap development source tree
NETMAPTREE=~/workspace/netmap/v2

# Picobsd source tree
PICOTREE=${KERNELTREE}/release/picobsd

# Picobsd image
IMAGE=netmap-qemu
# for which architecture
ARCH=i386


# Utility function which output a given command before executing it
function echoandexec
{
    echo "$@"
    "$@"
}

# Initialize the picobsd development environment
function pinit
{
    local arch=${1:-${ARCH}}

    read -p "Build kernel toolchain before? [N/y] "
    [ "${REPLY}" = y ] && (
        cd ${KERNELTREE}
        make kernel-toolchain
    )

    read -p "Proceed building picobsd tree (${arch}) from scratch? [N/y] "
    [ "${REPLY}" = 'y' ] && (
        cd ${PICOTREE}/build
        echoandexec ./picobsd --src ${KERNELTREE} --arch ${arch} --init
    )
}

# Create the picobsd image, seriously!
function do_build
{
    local arch=${1:-${ARCH}}
    local image=${2:-${IMAGE}}

    (
    cd ${PICOTREE}/build
    echoandexec ./picobsd --src ${KERNELTREE}  --arch ${arch} -n ${image}
    )
}

# Show image settings, build externals applications, and finally create the
# picobsd image
function pbuild
{
    local netmap=${KERNELTREE}/sys/net/netmap.h
    local netmap_kern=${KERNELTREE}/sys/dev/netmap/netmap_kern.h
    local loader=${PICOTREE}/${IMAGE}/floppy.tree/boot/loader.conf
    local sysctl=${PICOTREE}/${IMAGE}/floppy.tree/etc/sysctl.conf

    echo ${netmap}
    grep 'NETMAP_BUF_SIZE' ${netmap}
    echo

    echo ${netmap_kern}
    grep '^#define NETMAP_LUT' ${netmap_kern}
    grep '^#define CONSERVATIVE' ${netmap_kern}
    grep '^#define NETMAP_SKIP_POLL' ${netmap_kern}
    grep '^#define NETMAP_SLOW_POLL' ${netmap_kern}
    grep '^#define NETMAP_DOUBLE_PACKETS' ${netmap_kern}
    grep '^#define NETMAP_LATENCY_TIMESTAMPS' ${netmap_kern}
    echo

    echo ${loader}
    grep '^[^#].*' ${loader} 
    echo

    echo ${sysctl}
    grep '^[^#].*' ${sysctl} 
    echo

    read -p "Press any key to continue... "

    # build external binaries
    echoandexec make INCLUDES=/usr/include/ clean all -C ${NETMAPTREE} || return 1

    do_build "$@"
}

# Clean previously built image (its name and architecture are optional)
function pclean()
{
    local arch=${1:-${ARCH}}
    local image=${2:-${IMAGE}}

    (
        cd ${PICOTREE}/build
        echoandexec ./picobsd -c --src ${KERNELTREE} --arch ${arch} -n ${image}
    )
}

# Fire up a QEMU virtual machine with the given picobsd image.
function ptest
{
    local id=${1:-0}
    local model=${2:-e1000}
    local arch=${3:-${ARCH}}
    local image=${4:-${IMAGE}}
    local bin=${PICOTREE}/build/build_dir-${image}-${arch}/picobsd.bin

    echoandexec qemu ${bin} -m 512M \
           -net nic,model=${model},macaddr=52:54:00:12:34:5${id} \
           -net tap,ifname=tap${id},script=no
}

# Dump the picobsd image on a usb stick
function pdump
{
    local arch=${1:-${ARCH}}
    local image=${2:-${IMAGE}}
    local usb=${3:-/dev/da0}
    local bin=${PICOTREE}/build/build_dir-${image}-${arch}/picobsd.bin

    echoandexec sudo dd bs=512K if=${bin} of=${usb}
}


# entry point
command=$1
case "$1" in
init|build|clean|test|dump)
    shift
    p${command} "$@"
    ;;

*)
    echo 'WTF?!'
esac
