# functions to switch from LVDS1 to VGA and vice versa
function ActivateDP {
    echo "Switching to DP-1"
    xrandr --auto --output DP-1 --mode 1920x1080 --above LVDS-1
}
function DeactivateDP {
    echo "Switching to LVDS-1"
    xrandr --output DP-1 --off --output LVDS-1 --mode 1366x768
}
alias menukey='xmodmap -e "keycode 107 = Menu"'

function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit, working tree clean" ]] && echo "*"
}
function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}
export PS1="\u@\h:\w \$(parse_git_branch)\$ " # shinichi@ayanami:~/source/cryptocoins [master]$
export GOPATH=~/source/go
export GOSRC=~/source/go/src/github.com
export PATH=$GOPATH/bin:$PATH

alias ez='nano ~/.bash_profile && source ~/.bash_profile'
alias grw='git commit --amend --no-edit'

# update qrl-docker images and remove all dangling fs layers
alias dockerqrlupdate='docker pull qrledger/qrl-docker:xenial; docker pull qrledger/qrl-docker:bionic && docker rmi $(docker images -f dangling=true -q)'

source ~/.env_secrets
