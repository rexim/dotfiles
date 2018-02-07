#!/bin/sh

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

pdf_to_pngs() {
    mkdir -p "$2"
    pdftoppm -rx 300 -ry 300 -png "$1" "$2/page"
}

invert_pngs() {
    pushd "$1"
    for page in page-*.png; do
        convert -negate "${page}" "neg-${page}"
        mv "neg-${page}" "${page}"
    done
    popd
}

pngs_to_oras() {
    pushd "$1"
    for page in page-*.png; do
        "${SCRIPT_DIR}/png_to_ora" "${page}"
        rm "${page}"
    done
    popd
}

if [[ $# -ne 2 ]]; then
    >&2 echo "Usage ./prepare-pdf-noting <pdf-file> <notes-folder>"
    exit 1
fi

set -xe

PDF_FILE="$1"
NOTES_FOLDER="$2"

pdf_to_pngs "${PDF_FILE}" "${NOTES_FOLDER}"
invert_pngs "${NOTES_FOLDER}"
pngs_to_oras "${NOTES_FOLDER}"
