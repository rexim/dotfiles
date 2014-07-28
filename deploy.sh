#!/bin/sh

SCRIPT_DIR="$( cd "$( dirname "$BASH_SOURCE[0]" )" && pwd )"

copyFile() {
    filename=$1
    if [ -f ~/$filename ] || [ -d ~/$filename ]; then
        echo "[WARNING] $filename is already copied"
    else
        cp -r $SCRIPT_DIR/$filename ~/$filename
        echo "[OK] $filename has been copied"
    fi
}

symlinkFile() {
    filename=$1
    if [ -L ~/$filename ]; then
        echo "[WARNING] $filename is already symlinked"
    else
        ln -s $SCRIPT_DIR/$filename ~/$filename
        echo "[OK] $filename has been symlinked"
    fi
}

for row in `cat MANIFEST`; do
    filename=`echo $row | cut -d \| -f 1`
    operation=`echo $row | cut -d \| -f 2`

    case $operation in
        copy)
            copyFile $filename
            ;;

        symlink)
            symlinkFile $filename
            ;;

        *)
            echo "[WARNING] Unknown operation $operation. Skipping..."
            ;;
    esac
done
