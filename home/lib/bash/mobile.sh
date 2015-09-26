ANDROID_SDK="${HOME}"/opt/android-sdk

android() { "${ANDROID_SDK}"/tools/android; }

adb() {
    local ADB="${ANDROID_SDK}"/platform-tools/adb
    case $1 in
        ls)
            ${ADB} devices -l
            ;;
        sc)
            local dest=${2:-screenshot.png}
            ${ADB} shell screencap -p | perl -pe 's/\x0D\x0A/\x0A/g' > ${dest}
            ;;
        *)
            ${ADB} "$@"
            ;;
    esac
}

# iOS emulator id
_iei() { xcrun simctl list devices | grep -v unavailable | grep "$@" | awk '{print $3}' | tr -d \(\); }


# Emulators
ea5() { ${ANDROID_SDK}/tools/emulator -avd nexus5 -scale 0.29 & }
ea6() { ${ANDROID_SDK}/tools/emulator -avd nexus6 -scale 0.26 & }
ei4s() { open -a "Simulator" --args -CurrentDeviceUDID $(_iei 'iPhone 4s'); }
ei5s() { open -a "Simulator" --args -CurrentDeviceUDID $(_iei 'iPhone 5s'); }
ei6() { open -a "Simulator" --args -CurrentDeviceUDID $(_iei 'iPhone 6'); }

# Titanium {{{

ti() { myreattach-to-user-namespace appc ti --no-color "$@"; }

tiba() { ti build --platform android "$@"; }
tibad() { ti build --platform android --target device "$@"; }
tiba5() { ti build --platform android --device-id nexus5 "$@"; }
tiba6() { ti build --platform android --device-id nexus6 "$@"; }

tibi() { ti build --platform ios "$@"; }
tibid() { tibi --target device; }
tibi4s() { tibi -C $(_iei 'iPhone 4s') "$@"; }
tibi5s() { tibi -C $(_iei 'iPhone 5s') "$@"; }
tibi6() { tibi -C $(_iei 'iPhone 6') "$@"; }

ticl() { ti clean; rm -rf build Resources; }

tilog() {
    local package=$(xpath tiapp.xml '//ti:app/id/text()')
    adb logcat | grep `adb shell ps | grep $package | cut -c10-15`
}

# }}}
