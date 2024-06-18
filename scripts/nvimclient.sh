#!/usr/bin/env bash

nvim --server ~/.cache/nvim/server.pipe --remote $1
nvim --server ~/.cache/nvim/server.pipe --remote-ui
