source $HOME/.env
source $HOME/.env_secrets
source $HOME/.aliases

function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit, working tree clean" ]] && echo "*"
}
function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}
export PS1="\u@\h:\w \$(parse_git_branch)\$ "  # shinichi@ayanami:~/source/cryptocoins [master]$

source /usr/local/bin/virtualenvwrapper.sh
