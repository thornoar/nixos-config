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

function orphan_m4a {
    for file in ./**/*.m4a; do
        if test ! -f "$file"; then
            continue
        fi
        pref="${file%*.m4a}"
        if test ! -f "$pref.lrc" -a ! -f "$pref.txt"; then
            echo "$file"
        fi
    done
}

function search_m4a {
    im4a=1
    for file in ./**/*.m4a; do
        if test ! -f "$file"; then
            continue
        fi
        pref="${file%*.m4a}"
        artist=""
        title=""
        if test ! -f "$pref.lrc" -a ! -f "$pref.txt"; then
            artist="$(ffprobe -v quiet -show_entries format_tags=artist -of default=noprint_wrappers=1:nokey=1 "$file")"
            title="$(ffprobe -v quiet -show_entries format_tags=title -of default=noprint_wrappers=1:nokey=1 "$file")"
            echo "$iflac $artist - $title"
            syncedlyrics "$artist - $title" --synced-only -o "$pref.lrc" > /dev/null
        fi
        if test ! -f "$pref.lrc" -a ! -f "$pref.txt"; then
            echo "-- trying plain lyrics"
            syncedlyrics "$artist - $title" --plain-only -o "$pref.txt" > /dev/null
        fi
        ((im4a++))
    done
}

function patch_m4a {
    for file in ./**/*.m4a; do
        if test ! -f "$file"; then
            continue
        fi
        pref="${file%*.m4a}"
        if test ! -f "$pref.lrc" -a ! -f "$pref.txt"; then
            echo "$file"
            echo "[au: instrumental]" > "$pref.lrc"
        fi
    done
}

function orphan_mp3 {
    for file in ./**/*.mp3; do
        if test ! -f "$file"; then
            continue
        fi
        pref="${file%*.mp3}"
        if test ! -f "$pref.lrc" -a ! -f "$pref.txt"; then
            echo "$file"
        fi
    done
}

function search_mp3 {
    imp3=1
    for file in ./**/*.mp3; do
        if test ! -f "$file"; then
            continue
        fi
        pref="${file%*.mp3}"
        artist=""
        title=""
        if test ! -f "$pref.lrc" -a ! -f "$pref.txt"; then
            artist="$(ffprobe -v quiet -show_entries format_tags=artist -of default=noprint_wrappers=1:nokey=1 "$file")"
            title="$(ffprobe -v quiet -show_entries format_tags=title -of default=noprint_wrappers=1:nokey=1 "$file")"
            echo "$iflac $artist - $title"
            syncedlyrics "$artist - $title" --synced-only -o "$pref.lrc" > /dev/null
        fi
        if test ! -f "$pref.lrc" -a ! -f "$pref.txt"; then
            echo "-- trying plain lyrics"
            syncedlyrics "$artist - $title" --plain-only -o "$pref.txt" > /dev/null
        fi
        ((imp3++))
    done
}

function patch_mp3 {
    for file in ./**/*.mp3; do
        if test ! -f "$file"; then
            continue
        fi
        pref="${file%*.mp3}"
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
    orphan_m4a
    orphan_mp3
fi

if [ $search == 1 ]; then
    search_flac
    search_m4a
    search_mp3
fi

if [ $patch == 1 ]; then
    patch_flac
    patch_m4a
    patch_mp3
fi
