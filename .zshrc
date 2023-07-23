# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/stu/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

#PS1='%n@%m %F{red}%/%f $ '
autoload -U colors && colors
PS1="%F{red}[%n@%m %1~]%{$reset_color%}%(#.#.$) "
PS2="> "
bindkey -v
bindkey ^R history-incremental-search-backward 
bindkey ^S history-incremental-search-forward
