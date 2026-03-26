#!/usr/bin/env bash

arch="32"
assemble=0

while [[ $# -gt 0 ]]; do
    case $1 in #(((
        -64) arch="64"; shift ;;
        -a) assemble=1; shift ;;
        *) POSITIONAL_ARGS+=("$1"); shift ;;
    esac
done
set -- "${POSITIONAL_ARGS[@]}"

if [ "$arch" = "64" ]; then
    target="mips64el-linux-gnu"
    qemucmd="qemu-mips64el"
else
    target="mipsel-linux-gnu"
    qemucmd="qemu-mipsel"
fi

file="$1"
# tmp="$(mktemp)"

echo "Assembling $file"
clang --target=$target -nostdlib -static -fuse-ld=lld -o "./a.out" "$file" || exit 1

if [ $assemble == 0 ]; then
    echo -e "\n\nRunning $qemucmd ./a.out\n----------------"
    $qemucmd "./a.out"
    rm ./qemu*
fi
