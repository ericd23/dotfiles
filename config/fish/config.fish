set -U fish_greeting
fish_vi_key_bindings

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
alias bs="sudo powertop --auto-tune"
alias grl="grep --recursive --line-number --binary-files=without-match --exclude-dir=node_modules --exclude-dir=.git"
alias d="cd $DOTS"
alias e=exit
alias pmd="pacman -Si"
alias pms="pacman -Ss"
alias pmf="pacman -Fl"
alias s="sudo su"
alias bm=bashmount
alias t="tmux new"
alias tl="tmux list-sessions"
alias tk="tmux kill-server"
alias red="sudo sh -c 'rmmod psmouse; modprobe psmouse proto=imps'"
alias tap="xinput --set-prop 'Synaptics TM3203-003' 'libinput Tapping Enabled' 1"

