#!/bin/sh

if [ `hostname` = "rexim-B590" ]; then
    ~/.screenlayout/portable-setup-b590.sh
else
    ~/.screenlayout/portable-setup-g50.sh
fi
