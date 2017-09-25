#!/bin/bash

BASE_URL="http://www.yawgatog.com/resources/oracle"
DATE="2017-09-20"
FORMATS="All%20Sets Vintage Legacy Modern Standard"

TARGET_DIR="${1}"

if [ -z "${TARGET_DIR}" ]; then
    TARGET_DIR="formats"
fi

function add-formats {
    local ARCHIVE

    mkdir "${TARGET_DIR}.tmp"
    cd "${TARGET_DIR}.tmp" || exit 3

    echo "Downloading sets..."
    for FORMAT in ${FORMATS}; do
        wget -nv "${BASE_URL}/${FORMAT}-${DATE}.zip"
    done

    for ARCHIVE in ./*.zip; do
        unzip "${ARCHIVE}"
    done

    for FORMAT in ./*.txt; do
        mv "${FORMAT}" "${FORMAT/[ 0-9-]*/}.cards"
    done

    for FORMAT in ./*.cards; do
        mv "${FORMAT}" "${FORMAT,,}"
    done

    for FORMAT in ./*.cards; do
        awk 'BEGIN { RS="\n\n"; FS="\n"; } { print $1; }' "${FORMAT}" \
            > "${FORMAT/cards/names}"
    done

    fmt -s -w 72 ./all.cards > all.cards.filled &&
        mv ./all.cards.filled ./all.cards

    cd - >/dev/null || exit 3
}

function swap-dirs {
    if [ -d "${TARGET_DIR}" ]; then
        mv "${TARGET_DIR}" "${TARGET_DIR}.bak.$(date -Is)"
    fi

    mkdir -p "${TARGET_DIR}"

    if [ -d "${TARGET_DIR}.tmp" ]; then
        mv ${TARGET_DIR}.tmp/*.names "${TARGET_DIR}/"
        mv "${TARGET_DIR}.tmp/all.cards" "${TARGET_DIR}/"
        rm -rf "${TARGET_DIR}.tmp"
    else
        echo "${TARGET_DIR}.tmp did not exist!"
    fi
}

add-formats && swap-dirs
