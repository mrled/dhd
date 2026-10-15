#!/usr/bin/env bash
# Standalone Bash implementation: three Braille cells wide, two lines high.
set -euo pipefail

usage() {
    printf 'Usage: %s NUMBER [NUMBER ...]\nNumbers must be decimal integers from 0 to 9999.\n' "${0##*/}"
}
if [[ ${1-} == --help || ${1-} == -h ]]; then
    usage
    exit 0
fi
if (( $# == 0 )); then
    usage >&2
    exit 2
fi

# Validate everything before printing. Remove leading zeros without octal parsing.
numbers=()
for text in "$@"; do
    if [[ ! $text =~ ^[0-9]+$ ]]; then
        printf 'Invalid number: %s (expected 0–9999)\n' "$text" >&2
        exit 2
    fi
    text=${text#"${text%%[!0]*}"}
    text=${text:-0}
    if (( ${#text} > 4 )); then
        printf 'Invalid number: %s (expected 0–9999)\n' "$text" >&2
        exit 2
    fi
    numbers+=("$((10#$text))")
done

bits=(0 3 1 4 2 5 6 7)
powers=(1 10 100 1000)
for row in 0 1; do
    separator=''
    for number in "${numbers[@]}"; do
        printf '%s' "$separator"
        separator=' '
        for column in 0 1 2; do
            mask=0
            for y in 0 1 2 3; do
                for x in 0 1; do
                    gx=$((column * 2 + x))
                    gy=$((row * 4 + y))
                    on=0
                    if (( gx == 2 )); then
                        on=1
                    elif (( gx < 5 && gy != 3 && gy != 4 )); then
                        if (( gx < 2 )); then dx=$((2 - gx)); else dx=$((gx - 2)); fi
                        if (( row == 0 )); then dy=$gy; else dy=$((7 - gy)); fi
                        place=$((row * 2 + (gx < 2)))
                        digit=$((number / powers[place] % 10))
                        if (( ((digit == 1 || digit == 7 || digit == 9) && dy == 0) ||
                              ((digit == 2 || digit == 8 || digit == 9) && dy == 2) ||
                              (digit == 3 && dx == dy) ||
                              (digit == 4 && dx + dy == 2) ||
                              (digit == 5 && dx == 2 && dy == 1) ||
                              (digit == 6 && dx == 2 && (dy == 0 || dy == 2)) ||
                              ((digit == 7 || digit == 8 || digit == 9) && dx == 2) )); then
                            on=1
                        fi
                    fi
                    if (( on )); then mask=$((mask | (1 << bits[y * 2 + x]))); fi
                done
            done
            # Encode U+2800 + mask directly as UTF-8, independent of locale.
            code=$((0x2800 + mask))
            printf -v encoded '\\%03o\\%03o\\%03o' \
                "$((0xe0 | (code >> 12)))" \
                "$((0x80 | ((code >> 6) & 63)))" \
                "$((0x80 | (code & 63)))"
            printf '%b' "$encoded"
        done
    done
    printf '\n'
done
