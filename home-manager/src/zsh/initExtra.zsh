autoload -U colors && colors
PS1="[%{$fg[red]%}%n%{$reset_color%}] %{$fg[yellow]%}%2~ %{$reset_color%}: "

function preexec() {
    timer=$(($(date +%s%0N)/1000000))
}

function precmd() {
    echo -ne '\e[4 q'
    if [ $timer ]; then
    now=$(($(date +%s%0N)/1000000))
    elapsed=$(($now-$timer))
    export RPROMPT="< %{$fg[yellow]%}${elapsed}ms%{$reset_color%}"
    unset timer
    fi
}

bindkey "^[[1;3D" backward-word 
bindkey "^[[1;3C" forward-word

# typeset -U PATH path
# BINPATH="$PROJECTS"
# path+=("$BINPATH" "${BINPATH}"/*/bin)
# export PATH

eval "$(fzf --zsh)"
bindkey "^[[1;5B" fzf-file-widget
bindkey "^[[1;5C" fzf-cd-widget
bindkey "^[[1;5A" fzf-history-widget
export FZF_CTRL_T_OPTS="--preview 'bat -n --color=always --line-range :500 {}'"
export FZF_ALT_C_OPTS="--preview 'eza --tree --color=always {} | head -200'"
_fzf_comprun() {
    local command=$1
    shift
    # ((((
    case "$command" in
        cd) fzf --preview "eza --tree --color=always {} | head -200" "$@" ;;
        export|unset) fzf --preview "eval 'echo \$'{}" "$@" ;;
        ssh) fzf --preview "dig {}" "$@" ;;
        *) fzf --preview "--preview 'bat -n --color=always --line-range :500 {}'" "$@" ;;
    esac
}

TIMEFMT=$'\n'\
'time:          %U user %S system %P cpu %*E total'$'\n'\
'max memory:    %M '$MAX_MEMORY_UNITS'MB'
