#!/usr/bin/env bash

shopt -s globstar

i=1

# for file in ./**/*.mp3 ./**/*.m4a ./**/*.flac; do
for file in ./**/*.mp3; do
    pref="${file%*.mp3}"

    # if test -f "$pref.txt"; then
    if test ! -f "$pref.lrc" -a -f "$pref.txt"; then
    # if test ! -f "$pref.flac" -a ! -f "$pref.mp3" -a ! -f "$pref.m4a"; then

        # artist="$(metaflac --show-tag=ARTIST "$file" | awk -F'=' '{print $2}')"
        # title="$(metaflac --show-tag=TITLE "$file" | awk -F'=' '{print $2}')"
        # artist="$(ffprobe -v quiet -show_entries format_tags=artist -of default=noprint_wrappers=1:nokey=1 "$file")"
        # title="$(ffprobe -v quiet -show_entries format_tags=title -of default=noprint_wrappers=1:nokey=1 "$file")"

        echo "$file"

        # del "$pref.txt"
        # echo "[au: instrumental]" > "$pref.lrc"

        # echo "$artist"
        # echo "$title"
        # syncedlyrics "$artist - $title" --synced-only -o "$pref.lrc" > /dev/null
        # echo -e "\n$i\n"
        # ((i++))
    fi
done

# find . -type f -name "*.$ext" -exec bash -c "process_file \"{}\"" \;

# 955
