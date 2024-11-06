export PROJECTS=$HOME/projects
export MEDIA=$HOME/media
export PATH=$PROJECTS/scripts:$PROJECTS/.config:$PATH:$PROJECTS/programming/.venv/bin
export NVIM_LISTEN_ADDRESS=/tmp/nvimsocket
export ZDOTDIR="$HOME/.config/zsh"
export XDG_DATA_HOME=$HOME/.local/share
export XDG_CONFIG_HOME=$HOME/.config
export XDG_STATE_HOME=$HOME/.local/state
export XDG_CACHE_HOME=$HOME/.cache
export DE="generic"
export CUDA_CACHE_PATH="$XDG_CACHE_HOME"/nv
export HISTFILE=$XDG_CONFIG_HOME/zsh/.zhistory
export TEXINPUTS=.:$PROJECTS/libs:$TEXINPUTS
export LAPTOP=false
export PYTHONPATH=/usr/share/asymptote:$PYTHONPATH

# File and Dir colors for ls and other outputs
export LS_OPTIONS='--color=auto'
export LESS_TERMCAP_mb=$'\E[01;32m'
export LESS_TERMCAP_md=$'\E[01;32m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;47;34m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;36m'
export LESS=-R

exec startx > /dev/null 2>&1
