# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/stu/.zshrc'

ulimit -c unlimited

autoload -Uz compinit
compinit
# End of lines added by compinstall

autoload -U colors && colors

autoload -Uz vcs_info
precmd() {
    vcs_info
}
zstyle ':vcs_info:*' formats ' %F{magenta}%b%f'

setopt prompt_subst

PS1='[%n@%m %F{cyan}%1~%f${vcs_info_msg_0_}]%(#.#.$) '
PS2="> "
bindkey -v
bindkey ^R history-incremental-search-backward 
bindkey ^S history-incremental-search-forward
bindkey jk vi-cmd-mode

export PYTHONPATH=$PYTHONPATH:~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/etc
export RUST_BACKTRACE=1
export EDITOR=vim
