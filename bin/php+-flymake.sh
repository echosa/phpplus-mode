#!/usr/bin/env bash

FILENAME=$1

php -l -f "$FILENAME"

phpcs --report=emacs "$FILENAME" $2

phpmd "$FILENAME" $3 $4

exit 0
