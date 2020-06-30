function parse_git_dirty {
[[ $(git status 2> /dev/null | tail -n1) != "nothing to commit, working tree clean" ]] && echo "*"
}
function parse_git_branch {
git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}

function encode_m4a {
for file in "$@"
do
    faac -q 150 -w --overwrite $file
done
}

# Like Spotlight. But, it might not list every file because it uses mlocate. Also doesn't work well with sudo.
function o {
FILEORDIR="$(locate "$@" | fzf)"

if [ -f "$FILEORDIR" ]
then
    rifle "$FILEORDIR"
else
    cd "$FILEORDIR"
fi
}

# This function needed because gnome-terminal doesn't let you GUI rename!
function set-title() {
  if [[ -z "$ORIG" ]]; then
    ORIG=$PS1
  fi
  TITLE="\[\e]2;$*\a\]"
  PS1=${ORIG}${TITLE}
}

export PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$(parse_git_branch) " # shinichi@ayanami:~/source/cryptocoins [master]$
export GOPATH=~/source/go
export GOBIN=~/source/go/bin
export PATH=$GOPATH/bin:~/.local/bin:$PATH
export LIBVA_DRIVER_NAME=iHD

alias ll='exa -l'
alias lt='exa -T'
alias ez='nano ~/.bash_profile && source ~/.bash_profile'
alias grw='git commit --amend --no-edit'
alias gs='git status'
alias gd='git diff'
alias gcm='git commit -m'
alias gco='git checkout'

alias dockerclean='docker rm $(docker ps -aq)'
alias vpn='cd ~/.ironsocket && sudo openvpn Netherlands.ovpn'
alias vpnjpn='cd ~/.ironsocket && sudo openvpn Japan.ovpn'

alias listinstalledpackages="pacman -Qent"
alias findpackage='dpkg -S'
source ~/.env_secrets

export FZF_CTRL_T_COMMAND='find .'
export FZF_ALT_C_COMMAND='find .'
source /usr/share/fzf/key-bindings.bash
source /usr/share/fzf/completion.bash
source /usr/bin/virtualenvwrapper.sh
