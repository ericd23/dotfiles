export PATH=~/.local/bin:~/.config/doom/bin:~/.config/emacs/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin
export SHELL=fish
export EDITOR=vis
export VISUAL=vis
export PAGER=less
export BROWSER=firefox
export XDG_CONFIG_HOME=~/.config
export XDG_CACHE_HOME=~/.cache
export XDG_DATA_HOME=~/.local/share
export PASSWORD_STORE_DIR=${XDG_DATA_HOME:-~/.local/share}/pass
export GNUPGHOME=${XDG_CONFIG_HOME:-~/.config}/gnupg
export EMACSDIR=${XDG_CONFIG_HOME:-~/.config}/emacs
export DOOMDIR=${XDG_CONFIG_HOME:-~/.config}/doom
export DOTS=~/.dotfiles
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

[ "$(uname)" = "Linux" ] && [ "$(tty)" = "/dev/tty1" ] && \
! pidof -s Xorg >/dev/null 2>&1 && \
exec startx
