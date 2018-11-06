# functions to switch from LVDS1 to VGA and vice versa
DP='DP-1'
LVDS='LVDS-1'
function ActivateDP {
    echo "Switching to $DP"
    xrandr --auto --output $DP --auto --above $LVDS
}
function DeactivateDP {
    echo "Switching to $LVDS"
    xrandr --output $DP --off --output $LVDS --auto
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

export PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$(parse_git_branch) " # shinichi@ayanami:~/source/cryptocoins [master]$
export GOPATH=~/source/go
export GOSRC=~/source/go/src/github.com
export PATH=$GOPATH/bin:$PATH
export ZEPHYR_SDK_INSTALL_DIR="/opt/zephyr-sdk"
export ZEPHYR_TOOLCHAIN_VARIANT="zephyr"


alias ez='nano ~/.bash_profile && source ~/.bash_profile'
alias grw='git commit --amend --no-edit'
alias gs='git status'
alias any='cd ~/source/anyledger && ls'

# update qrl-docker images and remove all dangling fs layers
alias dockerqrlupdate='docker pull qrledger/qrl-docker:xenial; docker pull qrledger/qrl-docker:bionic && docker rmi $(docker images -f dangling=true -q)'
alias dockerclean='docker rm $(docker ps -aq)'

alias zephyrninja='mkdir $ZEPHYR_ARCH && cd $ZEPHYR_ARCH && cmake -GNinja -DBOARD=$ZEPHYR_ARCH ../.. && ninja && ninja run'
alias listinstalledpackages="comm -23 <(apt-mark showmanual | sort -u) <(gzip -dc /var/log/installer/initial-status.gz | sed -n 's/^Package: //p' | sort -u)"
source ~/.env_secrets

