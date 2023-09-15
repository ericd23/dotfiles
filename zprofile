# login sessions profile

export PATH=~/.local/bin:$PATH
export SHELL=zsh
export EDITOR=vim
export VISUAL=vim
export PAGER=less
export BROWSER=brave
export DOTS=~/.dotfiles
export XDG_CONFIG_HOME=~/.config
export XDG_CACHE_HOME=~/.cache
export XDG_DATA_HOME=~/.local/share
export XDG_STATE_HOME=~/.local/state

[ -f ~/.loginprofile ] && . ~/.loginprofile
