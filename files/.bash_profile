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

# fzf_cmd - run command on specific directory, adapted from __fzf__cd__()
function fzf_cmd() {
  local dir
  dir=$(fd --type d . $2  2> /dev/null | fzf +m) &&
  printf "$1 %q" "$dir"
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
export PATH=~/.cargo/bin:./node_modules/.bin:$GOPATH/bin:~/.local/bin:$PATH
export LIBVA_DRIVER_NAME=iHD

alias pc='sudo pacman'
alias ll='exa -l'
alias lt='exa -T'
alias ez='nano ~/.bash_profile && source ~/.bash_profile'
alias grw='git commit --amend --no-edit'
alias gs='git status'
alias gd='git diff'
alias gcm='git commit -m'
alias gca='git commit --amend'
alias gco='git checkout'
alias grc='git rebase --continue'
alias open='rifle'

alias dockerclean='docker rm $(docker ps -aq)'
alias vpn='cd ~/.ironsocket && sudo openvpn Netherlands.ovpn'
alias vpnjpn='cd ~/.ironsocket && sudo openvpn Japan.ovpn'
alias sshfsfranziska='sshfs shinichi@franziska:/data ~/mnt'
alias wakeupg5='wol 00:14:51:66:f0:54'
alias listinstalledpackages="pacman -Qent"
source ~/.env_secrets
source /usr/bin/virtualenvwrapper.sh

export FZF_ALT_C_COMMAND='find ~/source/ -type d ! -path "*/\.git/*" ! -path "*/node_modules/*"'
source /usr/share/fzf/key-bindings.bash
source /usr/share/fzf/completion.bash
bind -m emacs-standard '"\ew": " \C-b\C-k \C-u`fzf_cmd cd ~/source`\e\C-e\er\C-m\C-y\C-h\e \C-y\ey\C-x\C-x\C-d"'
bind -m emacs-standard '"\ec": " \C-b\C-k \C-u`fzf_cmd code ~/source`\e\C-e\er\C-m\C-y\C-h\e \C-y\ey\C-x\C-x\C-d"'

# Vocdoni stuff
alias buildvocone='pushd . && cd ~/source/work/vocdoni-node && go build -gcflags="all=-N -l" ./cmd/voconed && popd'
alias runvocone='cd ~/source/work/vocdoni-node && rm -rf ~/.voconed && ./voconed --treasurer=0xfe10DAB06D636647f4E40dFd56599da9eF66Db1c --oracle=6aae1d165dd9776c580b8fdaf8622e39c5f943c715e20690080bbfce2c760223 --txCosts 20 --logLevel=debug'
alias runvochaintest='cd ~/source/work/vocdoni-node && ./vochaintest --operation tokentransactions --treasurerKey d0b9e29f07816fdd0ecc43f494b52beeb1ccf5e868c1634afec9d0e51af1cc37 --gwHost http://localhost:9095/dvote
'
alias rebuild-dvote-js='pushd . && cd ~/source/work/dvote-js && npm run build && npm pack && popd && npm install ~/source/work/dvote-js/dvote-js-1.16.2.tgz'
alias prcheck='cd ~/source/work/vocdoni-node && staticcheck ./... && go vet ./...'
