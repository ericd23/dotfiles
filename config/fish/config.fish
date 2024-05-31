if status is-interactive
    # Commands to run in interactive sessions can go here
    atuin init fish | source
    fish_vi_key_bindings default

    alias l="ls -alFh"
    alias lt="ls -alFht"
    alias ga="git add"
    alias gd="git diff"
    alias gst="git status"
    alias gcmsg="git commit -m"
    alias gss="git status -s"
    alias gds="git diff --staged"
    alias gl="git log --pretty=oneline"
    alias glg="git log --graph --pretty=format:'%h/%an - %s'"
    alias gus="git restore --staged"
    alias gdog="git log --all --decorate --oneline --graph"
    alias gb="git branch"
    alias ..="cd .."
    alias grl="grep --recursive --line-number --binary-files=without-match --exclude-dir=node_modules --exclude-dir=.git"
    alias d="cd $DOTS"
    alias e=exit
    alias pmd="pacman -Si"
    alias pms="pacman -Ss"
    alias pmf="pacman -Fl"
    alias bm=bashmount
    alias sc="systemctl"
    alias scu="systemctl --user"
    alias n=nvim
    alias para="lf ~/Documents/PARA"
    alias g=gitui
    alias pp="pass git push"
    alias sdu="systemctl --user"

    direnv hook fish | source
end
