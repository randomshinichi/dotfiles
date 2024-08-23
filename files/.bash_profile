function parse_git_dirty {
[[ $(git status 2> /dev/null | tail -n1) != "nothing to commit, working tree clean" ]] && echo "*"
}
function parse_git_branch {
git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}

function inaudible {
    local filename="$1"
    local filename_new_ext="${filename%.*}.m4b"
    ffmpeg -y -activation_bytes $AUDIBLE_ACTIVATIONBYTES -i $filename -codec copy $filename_new_ext
}

function open {
    nohup xdg-open $1 > /dev/null 2>&1 &
}

function dinto {
    docker exec -it "$1" /bin/sh
}

function encode_m4a {
for file in "$@"
    do
        # Assuming the output file should have the same name but with a .m4a extension
        output_file="${file%.*}.m4a"

        # -y is for 'yes, overwrite'
        ffmpeg -y -i "$file" -c:a aac -q:a 150 "$output_file"
    done
}

function docker_fzf() {
    local selected_container
    selected_container=$(docker ps -a --format '{{.Names}}' | fzf)
    READLINE_LINE="${READLINE_LINE}${selected_container}"
    READLINE_POINT=$((READLINE_POINT + ${#selected_container}))
}

function generic_fzf() {
    local selected_item
    selected_item=$(eval "$1")
    READLINE_LINE="${READLINE_LINE}${selected_item}"
    READLINE_POINT=$((READLINE_POINT + ${#selected_item}))
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
export PATH=~/.fly/bin:~/.local/share/solana/install/active_release/bin:~/.cargo/bin:$GOPATH/bin:~/.local/bin:./node_modules/.bin:~/.config/emacs/bin:$PATH
export CARGO_BUILD_JOBS=6

alias df='df -xtmpfs -xdevtmpfs -xefivarfs'
alias pc='sudo pacman'
alias pcq='sudo pacman --noconfirm'
alias ls='ls --color'
alias ll='exa -l'
alias lt='exa -T'
alias ez='nano ~/.bash_profile && source ~/.bash_profile'
alias en='sudo nano /etc/nixos/configuration.nix'
alias grw='git commit --amend --no-edit'
alias gs='git status'
alias gd='git diff'
alias gcm='git commit -n -m'
alias gca='git commit --amend'
alias gco='git checkout'
alias grc='git rebase --continue'
alias grs='git restore --staged'

get_repo_name() {
  echo "$1" \
    | sed -E 's/.*:\/\/[^/]+\/(.*?)\.git.*/\1/' \
    | tr '[:upper:]' '[:lower:]'
}
gitmp() {
  cd ~/source/work/tmp || exit
  git clone "$1"
  subl "$(basename "$1" .git)"
}
alias gitget='cd ~/source/work/tmp && git clone'

alias dockerclean='docker rm $(docker ps -aq)'
alias dockervclean='docker volume rm $(docker volume ls -q)'
alias vpn='cd ~/.purevpn/TCP && sudo openvpn ua2-ovpn-tcp.ovpn'
alias sshfsfranziska='sshfs shinichi@franziska:/data ~/mnt'
alias wakeupg5='wol 00:14:51:66:f0:54'
alias listinstalledpackages="pacman -Qent"

alias dpsa='docker ps -a'
alias dpsan="docker ps -a --format '{{.Names}}\t{{.Status}}\t{{.Ports}}'"
alias dlf='docker logs --follow'
alias nmssave='sshfs steamdeck:"/home/deck/Games/Heroic/Prefixes/default/No Mans Sky/pfx/drive_c/users/steamuser/AppData/Roaming/HelloGames" ~/mnt'

export FZF_ALT_C_COMMAND='find ~/source/ -type d ! -path "*/\.git/*" ! -path "*/node_modules/*"'
export PAGER=less
source /usr/bin/virtualenvwrapper_lazy.sh
source /usr/share/fzf/key-bindings.bash
source /usr/share/nvm/init-nvm.sh
bind -m emacs-standard '"\ew": " \C-b\C-k \C-u`fzf_cmd cd ~/source/work`\e\C-e\er\C-m\C-y\C-h\e \C-y\ey\C-x\C-x\C-d"'
bind -m emacs-standard '"\ec": " \C-b\C-k \C-u`fzf_cmd code ~/source/work`\e\C-e\er\C-m\C-y\C-h\e \C-y\ey\C-x\C-x\C-d"'
bind -m emacs-standard '"\es": " \C-b\C-k \C-u`fzf_cmd subl ~/source/work`\e\C-e\er\C-m\C-y\C-h\e \C-y\ey\C-x\C-x\C-d"'
bind -x '"\ee": "generic_fzf '\''docker ps -a --format {{.Names}} | fzf'\''"'
bind -x '"\eo": "generic_fzf '\''git branch | fzf'\''"'

source ~/.env_secrets
source ~/source/work/sync/mtstools/jupiter-shortcuts
