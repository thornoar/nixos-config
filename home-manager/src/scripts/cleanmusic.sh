#!/usr/bin/env bash

# The music library protocol:
# - All music files must be in `flac` format.
# - A music file `name.flac` must have a `name.lrc` or `name.txt` in the same directory.
# - Every directory containing music files must have a `cover.jpg` file.
# - The PICTURE metadata block of every music file must coincide with the `cover.jpg` in the same directory.
# - No directory may contain both files and directories.

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

function find_covers {
    find . -type d -print0 | while IFS= read -r -d '' dir; do
        if [ -z "$(find "$dir" -mindepth 1 -maxdepth 1 -type d -print -quit)" ]; then
            if test ! -f "$dir/cover.jpg"; then
                echo "$dir"
                for file in "$dir"/*.flac; do
                    if test ! -f "$file"; then
                        break
                    fi
                    metaflac --export-picture-to="$dir/cover.jpg" "$file" && break
                done
            fi
        fi
    done
}

function update_covers {
    find . -type d -print0 | while IFS= read -r -d '' dir; do
        if [ -z "$(find "$dir" -mindepth 1 -maxdepth 1 -type d -print -quit)" ]; then
            if test -f "$dir/cover.jpg"; then
                for file in "$dir"/*.flac; do
                    metaflac --remove --block-type=PICTURE "$file"
                    metaflac --import-picture-from="$dir/cover.jpg" "$file"
                done
            fi
        fi
    done
}

function update_covers_indiv {
    for file in ./*.flac; do
        echo "$file"
        metaflac --export-picture-to tmp.jpg "$file"
        magick ./tmp.jpg -crop 720x+280+0 tmp.jpg
        file ./tmp.jpg
        metaflac --remove --block-type=PICTURE "$file"
        metaflac "$file" --import-picture-from tmp.jpg
        rm ./tmp.jpg
        echo ""
    done
}

orphan=0
search=0
patch=0
cover=0
update=0
indiv=0
msg=0
while getopts "ospcuyh" option; do
    case "$option" in # (((((
        o) orphan=1 ;;
        s) search=1 ;;
        p) patch=1 ;;
        c) cover=1 ;;
        u) update=1 ;;
        y) indiv=1 ;;
        h) msg=1 ;;
        *) exit 1 ;;
    esac
done

if [ $msg == 1 ]; then
    printf "usage: cleanmusic [ -o -s -p -c -u -y -h ]\n"
    printf "flags:\n"
    printf "  -o  list all songs in CWD without a lyrics file\n"
    printf "  -s  search lyrics files for all songs in CWD\n"
    printf "  -p  add the [au:instrumental] tag to all songs without lyric files\n"
    printf "  -c  generate \`cover.jpg\` files from metadata\n"
    printf "  -u  write \`cover.jpg\` files to the music metadata\n"
    printf "  -y  used to process the covers of YM-downloaded songs\n"
    printf "  -h  show this help message\n"
    exit 0
fi

if [ $orphan == 1 ]; then
    orphan_flac
fi

if [ $search == 1 ]; then
    search_flac
fi

if [ $patch == 1 ]; then
    patch_flac
fi

if [ $cover == 1 ]; then
    find_covers
fi

if [ $update == 1 ]; then
    update_covers
fi

if [ $indiv == 1 ]; then
    update_covers_indiv
fi
