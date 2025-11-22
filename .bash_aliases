# -*- Mode: shell-script -*-

alias rm="rm -i"
alias cp="cp -i"

export PAGER=less
unset LESSOPEN

export EDITOR=vim
set -o vi
bind '"jk":vi-movement-mode'

