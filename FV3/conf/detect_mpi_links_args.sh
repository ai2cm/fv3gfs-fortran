#!/bin/bash

function mpichLDFlags {
    # -show prints includes as well as the exe...remove these
    if command -v mpifort &> /dev/null
    then
	mpifort -show | sed 's/\(-I[^ ]*\|^[^ ]*\)//g'
    elif command -v ftn &> /dev/null
    then
	ftn -craype-verbose | sed 's/\(-I[^ ]*\|^[^ ]*\)//g'# | sed '$p'
    else
	echo "Could not find mpifort or ftn"
    fi
}

function openmpiLDFlags {
    if command -v mpif90 &> /dev/null
    then
	mpif90 --showme:link
    fi
}


mpichLDFlags || openmpiLDFlags 2> /dev/null
