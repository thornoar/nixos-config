autoload -U colors && colors
PS1="[%{$fg[red]%}%n%{$reset_color%}] %{$fg[yellow]%}%2~ %{$reset_color%}: "

function precmd() {
    echo -ne '\e[4 q'
    unset MYVIMRC
}

bindkey "^[[1;3D" backward-word 
bindkey "^[[1;3C" forward-word

eval "$(fzf --zsh)"
bindkey "^[[1;5B" fzf-file-widget
bindkey "^[[1;5C" fzf-cd-widget
bindkey "^[[1;5A" fzf-history-widget

alias -- clip='wl-copy -n'
alias -- close=exit
alias -- decrypt='openssl enc -aes-256-cfb -iter 100 -a -d'
alias -- def='dict -h dict.org'
alias -- encrypt='openssl enc -aes-256-cfb -iter 100 -a'
alias -- film='transmission-remote -w ~/media/films -a '
alias -- gitlog='git log --oneline --graph --decorate'
alias -- gpp='g++ -std=c++11 -Wall -fsanitize=leak,address,undefined'
alias -- grep='grep --color=auto'
alias -- lbr='clear; br'
alias -- lsbr='br -c ":print_tree"'
alias -- music='transmission-remote -w ~/media/music -a '
alias -- open=xdg-open
alias -- torrent=transmission-remote
alias -- vmcon='virt-manager --connect qemu:///system --show-domain-console'
alias -- fdate='date +"%d %b %Y (%a): %H:%M"'
alias -- develop='export ZDOTDIR="$XDG_CONFIG_HOME/nix-develop" && nix develop'
