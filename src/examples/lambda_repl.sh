#!/bin/sh -e
echo " _                    _         _
| |    __ _ _ __ ___ | |__   __| | __ _
| |   / _\` | '_ \` _ \| '_ \\ / _\` |/ _\` |
| |__| (_| | | | | | | |_) | (_| | (_| |
|_____\__,_|_| |_| |_|_.__/ \__,_|\__,_|
"
ACC=""
while true
do
    echo "> \c"
    read -r IN
    if [ -n "$ACC" ]; then ACC="$ACC
$IN"
    else ACC=$IN
    fi
    case $IN in
        main*) ./o "$ACC";;
        *);;
    esac
done
