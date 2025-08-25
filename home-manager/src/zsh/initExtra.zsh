autoload -U colors && colors
PS1="[%{$fg[red]%}%n%{$reset_color%}] %{$fg[yellow]%}%2~ %{$reset_color%}: "

bindkey "^[[1;3D" backward-word 
bindkey "^[[1;3C" forward-word

eval "$(fzf --zsh)"
bindkey "^[[1;5B" fzf-file-widget
bindkey "^[[1;5C" fzf-cd-widget
bindkey "^[[1;5A" fzf-history-widget
