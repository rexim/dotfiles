#!/bin/sh

SCRIPT_DIR="$( cd "$( dirname "$BASH_SOURCE[0]" )" && pwd )"
FILES="
.emacs
.emacs.rc
.emacs.snippets
"

for f in $FILES; do
    if [ -L ~/$f ]; then
        echo "[WARNING] $f is already deployed"
    else
        ln -s $SCRIPT_DIR/$f ~/$f
        echo "[OK] $f has been deployed"
    fi
done
