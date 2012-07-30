#!/usr/bin/env bash

function usage {
    echo >&2 "usage: $0 [-l] [-c phpcs_standard] [-m phpmd_format phpmd_rulesets] [file]"
    exit 1
}

LINT=off

CS=off
CS_STANDARD=""

MD=off
MD_FORMAT=""
MD_RULESET=""

while [ $# -gt 0 ]
do
    case "$1" in
        -l) LINT=on;;
        -c) CS=on; CS_STANDARD="$2"; shift;;
        -m) MD=on; MD_FORMAT="$2"; MD_RULESETS="$3"; shift; shift;;
        -*) usage;;
        *)  break;;
    esac
    shift
done


FILENAME=$1

if [ ! -e "$FILENAME" ]
then
    usage
fi

if [ "$LINT" == on ]
then
    php -l -f "$FILENAME"
fi

if [ "$CS" == on ]
then
    phpcs --report=emacs "$FILENAME" --standard="$CS_STANDARD"
fi

if [ "$MD" == on ]
then
    phpmd "$FILENAME" "$MD_FORMAT" "$MD_RULESETS"
fi

exit 0
