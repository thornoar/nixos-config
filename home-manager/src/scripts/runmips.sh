#!/usr/bin/env bash

file="$1"
tmp="$(mktemp)"
clang --target=mipsel-linux-gnu -nostdlib -static -fuse-ld=lld -o "$tmp" "$file"
qemu-mipsel "$tmp"
