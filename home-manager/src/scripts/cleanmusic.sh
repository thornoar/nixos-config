#!/usr/bin/env bash

shopt -s globstar

function orphan_flac {
    for file in ./**/*.flac; do
        if test ! -f "$file"; then
            continue
        fi
        pref="${file%*.flac}"
        if test ! -f "$pref.lrc" -a ! -f "$pref.txt"; then
            echo "$file"
        fi
    done
}

function search_flac {
    iflac=1
    for file in ./**/*.flac; do
        if test ! -f "$file"; then
            continue
        fi
        pref="${file%*.flac}"
        artist=""
        title=""
        if test ! -f "$pref.lrc" -a ! -f "$pref.txt"; then
            artist="$(metaflac --show-tag=ARTIST "$file" | awk -F'=' '{print $2}')"
            title="$(metaflac --show-tag=TITLE "$file" | awk -F'=' '{print $2}')"
            echo "$iflac $artist - $title"
            syncedlyrics "$artist - $title" --synced-only -o "$pref.lrc" > /dev/null
        fi
        if test ! -f "$pref.lrc" -a ! -f "$pref.txt"; then
            echo "-- trying plain lyrics"
            syncedlyrics "$artist - $title" --plain-only -o "$pref.txt" > /dev/null
        fi
        ((iflac++))
    done
}

function patch_flac {
    for file in ./**/*.flac; do
        if test ! -f "$file"; then
            continue
        fi
        pref="${file%*.flac}"
        if test ! -f "$pref.lrc" -a ! -f "$pref.txt"; then
            echo "$file"
            echo "[au: instrumental]" > "$pref.lrc"
        fi
    done
}

orphan=0
search=0
patch=0
while getopts "osp" option; do
    case "$option" in # (((((
        o) orphan=1 ;;
        s) search=1 ;;
        p) patch=1 ;;
        *) exit 1 ;;
    esac
done

if [ $orphan == 1 ]; then
    orphan_flac
fi

if [ $search == 1 ]; then
    search_flac
fi

if [ $patch == 1 ]; then
    patch_flac
fi
