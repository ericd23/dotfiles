export PATH=~/.local/bin:$PATH
export SHELL=zsh
export EDITOR=vis
export VISUAL=vis
export PAGER=less
export BROWSER=brave
export XDG_CONFIG_HOME=~/.config
export XDG_CACHE_HOME=~/.cache
export XDG_DATA_HOME=~/.local/share
export XDG_STATE_HOME=~/.local/state
export PASSWORD_STORE_DIR="$XDG_DATA_HOME"/pass
export GNUPGHOME="$XDG_CONFIG_HOME"/gnupg
export EMACSDIR="$XDG_CONFIG_HOME"/emacs
export CARGO_HOME="$XDG_DATA_HOME"/cargo
export CUDA_CACHE_PATH="$XDG_CACHE_HOME"/nv
export XAUTHORITY="$XDG_RUNTIME_DIR"/Xauthority
export GOPATH="$XDG_DATA_HOME"/go
export DOTS=~/.dotfiles
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java

export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export XMODIFIERS=@im=ibus

if [ "$(tty)" = "/dev/tty1" ]; then
    startx -- :0
elif [ "$(tty)" = "/dev/tty2" ]; then
    startx -- :1
fi
