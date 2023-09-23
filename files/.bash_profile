function parse_git_dirty {
[[ $(git status 2> /dev/null | tail -n1) != "nothing to commit, working tree clean" ]] && echo "*"
}
function parse_git_branch {
git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}

function dinto {
    docker exec -it "$1" /bin/sh
}

function encode_m4a {
for file in "$@"
do
    faac -q 150 -w --overwrite $file
done
}

function ffmpeg-yuvconvert {
    ffmpeg -i "$1" -c:v rawvideo -pixel_format yuv420p "$2"
}

function ffmpeg-cutoutduration {
    echo "Usage: ffmpeg-cutoutduration inputfile from(hh:mm:ss) length(hh:mm:ss) outfile"
    ffmpeg -i "$1" -ss $2 -t $3 -c:v copy -c:a copy "$4"
}

function ffmpeg-vmafcompare {
    echo "Usage: ffmpeg-vmafcompare transcoded original"
    ffmpeg -i "$1" -i "$2" -lavfi libvmaf='n_threads=8' -f null -
}

function docker_fzf() {
    local selected_container
    selected_container=$(docker ps -a --format '{{.Names}}' | fzf)
    READLINE_LINE="${READLINE_LINE}${selected_container}"
    READLINE_POINT=$((READLINE_POINT + ${#selected_container}))
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

function rundove-e2e {
  echo "checking if dove-deployer-1 exists"
  while true; do
    # dove-deployer-1 won't exist immediately when the docker-compose script is run, because it has to be built.
    if [ "$(docker ps -a --format '{{.Names}}' | grep -w 'dove-deployer-1')" ]; then
      echo "Container exists"
      break
    fi
    sleep 1
  done


  echo "waiting for dove-deployer-1 to exit"
  while true; do
    status=$(docker inspect dove-deployer-1 --format='{{.State.Status}}')
    if [ "$status" == "exited" ]; then
      exit_code=$(docker inspect dove-deployer-1 --format='{{.State.ExitCode}}')
      if [ "$exit_code" -eq 0 ]; then
        break
      else
        echo "dove-deployer-1 exited with an error"
        break
      fi
      break
    fi
    sleep 1  # wait for a short while before the next check
  done

  echo "Waiting for 'RESHARING: Current council' in dove-bird_0-1 logs"
  while true; do
    if docker logs dove-bird_0-1 2>/dev/null | grep -q "RESHARING: Current council"; then
      echo "'RESHARING: Current council' found in dove-bird_0-1 logs"
      cd $DOVE/test
      yarn $1
      break
    fi
    sleep 1
  done
}

export PS1="\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$(parse_git_branch) " # shinichi@ayanami:~/source/cryptocoins [master]$
export GOPATH=~/source/go
export GOBIN=~/source/go/bin
export PATH=~/.cargo/bin:$GOPATH/bin:~/.local/bin:$PATH
export CARGO_BUILD_JOBS=4

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
alias grs='git restore --staged'
alias open='rifle'

alias dockerclean='docker rm $(docker ps -aq)'
alias dockervclean='docker volume rm $(docker volume ls -q)'
alias vpn='cd ~/.purevpn/TCP && sudo openvpn de2-ovpn-tcp.ovpn'
alias sshfsfranziska='sshfs shinichi@franziska:/data ~/mnt'
alias wakeupg5='wol 00:14:51:66:f0:54'
alias listinstalledpackages="pacman -Qent"

alias dpsa='docker ps -a'
alias dpsan="docker ps -a --format '{{.Names}}\t{{.Status}}\t{{.Ports}}'"
alias dlf='docker logs --follow'
export DOVE="/home/shinichi/source/go/src/github.com/dove-foundation/dove"
alias rundove='sudo rm -rf pregenerated-dag/geth pregenerated-dag/keystore && BUILD=1 DAEMON=0 FASTBOOT=1 ./run_btc.sh'
alias lintdove='cd $DOVE/bird && golangci-lint run'
source ~/.env_secrets
source /usr/bin/virtualenvwrapper.sh

export FZF_ALT_C_COMMAND='find ~/source/ -type d ! -path "*/\.git/*" ! -path "*/node_modules/*"'
source /usr/share/fzf/key-bindings.bash
source /usr/share/fzf/completion.bash
bind -m emacs-standard '"\ew": " \C-b\C-k \C-u`fzf_cmd cd ~/source`\e\C-e\er\C-m\C-y\C-h\e \C-y\ey\C-x\C-x\C-d"'
bind -m emacs-standard '"\ec": " \C-b\C-k \C-u`fzf_cmd code ~/source`\e\C-e\er\C-m\C-y\C-h\e \C-y\ey\C-x\C-x\C-d"'
bind -x '"\ee": docker_fzf'

export PATH="/home/shinichi/.local/share/solana/install/active_release/bin:./node_modules/.bin:$PATH"
