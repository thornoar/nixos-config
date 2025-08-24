if [ "$(whoami)" = "ramak" ]; then
    source ~/.zshrc
fi
PS1="[%{$fg[blue]%}music%{$reset_color%}] %{$fg[yellow]%}%2~ %{$reset_color%}: "
