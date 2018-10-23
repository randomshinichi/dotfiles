# functions to switch from LVDS1 to VGA and vice versa
function ActivateDP {
    echo "Switching to DP-1"
    xrandr --auto --output DP-1 --mode 1920x1080 --above LVDS-1
}
function DeactivateDP {
    echo "Switching to LVDS-1"
    xrandr --output DP-1 --off --output LVDS-1 --mode 1366x768
}

function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit, working tree clean" ]] && echo "*"
}
function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}

function dotdiff {
  diff $1 ~/source/dotfiles/files/$1
}

export PS1="\u@\h:\w \$(parse_git_branch)\$ " # shinichi@ayanami:~/source/cryptocoins [master]$
export GOPATH=~/source/go
export GOSRC=~/source/go/src/github.com
export PATH=$GOPATH/bin:$PATH
export ZEPHYR_SDK_INSTALL_DIR="/opt/zephyr-sdk"
export ZEPHYR_TOOLCHAIN_VARIANT="zephyr"


alias ez='nano ~/.bash_profile && source ~/.bash_profile'
alias grw='git commit --amend --no-edit'
alias gs='git status'
alias any='cd ~/source/anyledger/anyledger-backend && ls && workon anybackend'

# update qrl-docker images and remove all dangling fs layers
alias dockerqrlupdate='docker pull qrledger/qrl-docker:xenial; docker pull qrledger/qrl-docker:bionic && docker rmi $(docker images -f dangling=true -q)'
alias dockerclean='docker rm $(docker ps -aq)'

alias zephyrninja='mkdir $ZEPHYR_ARCH && cd $ZEPHYR_ARCH && cmake -GNinja -DBOARD=$ZEPHYR_ARCH ../.. && ninja && ninja run'
source ~/.env_secrets

