export PATH=~/.local/bin:$PATH
export SHELL=fish
export EDITOR=vis
export VISUAL=vis
export PAGER=less
export BROWSER=firefox
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
export DOTS=~/.dotfiles

export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

[ "$(uname)" = "Linux" ] && [ "$(tty)" = "/dev/tty1" ] && \
! pidof -s Xorg >/dev/null 2>&1 && \
exec startx
