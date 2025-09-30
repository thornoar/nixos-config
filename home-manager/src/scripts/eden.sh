#!/usr/bin/env bash

if [ $# -lt 1 ]; then
    printf "A file argument is required.\n"
    exit 1
fi

case $1 in #((
    -*) printf "The first argument should be the file to edit!\n"
        exit 1
        ;;
    *) ;;
esac

file=$1; shift
name="$(basename "$file")"
if [[ "$name" == *.* ]]; then
    base="${name%.*}"
    ext="${name##*.}"
    if [[ "$ext" != "enc" ]]; then
        base="$name"
        ext=""
    fi
else
    base="$name"
    ext=""
fi
tmpfile="$(mktemp).$base"

if [[ "$*" == "" ]]; then
    args="-aes-256-cfb -iter 100 -a"
else
    args="$*"
fi

password=""
prompt="Enter password: "
echo -n "$prompt"
while IFS= read -r -s -n1 char; do
  [[ $char == "" ]] && break
  if [[ $char == $'\x7f' ]]; then
    if [[ -n $password ]]; then
      password="${password%?}"
      echo -ne "\b \b"
    fi
  else
    password+="$char"
    echo -n "*"
  fi
done
printf "\n"

if [ -e "$file" ]; then
    openssl enc $args -pass "pass:$password" -d -in "$file" -out "$tmpfile" || exit 1
    if [[ "$(file -b --mime-type "$tmpfile")" != "text/plain" ]]; then
        printf "Bad decryption (incorrect password or wrong encryption arguments)\n"
        exit 1
    fi
fi

$EDITOR "$tmpfile" || exit 1

read -r -p "Re-encrypt edited file? (y/n): " answer
if [[ "$answer" =~ ^[Yy]([Ee][Ss])?$ ]]; then
    openssl enc $args -pass "pass:$password" -in "$tmpfile" -out "$file"
    rm "$tmpfile"
else
    cat "$tmpfile"
    rm "$tmpfile"
fi
