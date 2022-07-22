#!/bin/bash

function mpichLDFlags {
    # -show prints includes as well as the exe...remove these
    ftn --cray-print-opts=all | sed 's/\(-I[^ ]*\|^[^ ]*\)//g'
}

function openmpiLDFlags {
    mpif90 --showme:link
}


mpichLDFlags || openmpiLDFlags 2> /dev/null
