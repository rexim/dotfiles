#!/bin/sh

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

ln -s $SCRIPT_DIR/.emacs ~/.emacs
ln -s $SCRIPT_DIR/.emacs.rc ~/.emacs.rc
ln -s $SCRIPT_DIR/.emacs.snippets ~/.emacs.snippets
