#!/bin/bash
ARGV=$@
ARGV=$#

COMPILE="yes"

for i in $ARGV; do
	case $i in
		"--no-compile")
			COMPILE="no"
			;;
		*)
			echo "unknown argument: $i"
			;;
	esac
done

if [ "$COMPILE" == "yes" ]; then
	lein figwheel :once
fi

function quit-error {
	echo "$1"
	exit 1
}

cd resources/public || quit-error "could not cd into resources/public"
zip -r lisp-gamejam-2020-COMPILE.zip ./*
