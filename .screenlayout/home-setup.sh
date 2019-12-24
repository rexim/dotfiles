#!/bin/sh

if [ `hostname` = "rexim-B590" ]; then
    ~/.screenlayout/home-setup-b590.sh
else
    ~/.screenlayout/home-setup-g50.sh
fi
