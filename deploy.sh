#!/bin/sh

SOURCE_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

ln -s $SOURCE_DIR/.emacs ~/.emacs
ln -s $SOURCE_DIR/.emacs.rc ~/.emacs.rc
