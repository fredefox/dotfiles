#!/usr/bin/env zsh

# Begin compinstall
zstyle :compinstall filename '$HOME/.zshrc'
autoload -Uz compinit -d "$XDG_DATA_HOME/zsh/zcompdump-$HOST-$ZSH_VERSION"
# End compinstall

setopt appendhistory extendedglob
unsetopt autocd beep
bindkey -e

autoload -U colors && colors
export PROMPT="$fg[blue]%1d %(?..%{$fg[red]%}%? )$fg[blue]§%{$reset_color%} "
export PS2="$fg[blue]%_> $reset_color"

alias sb="stack build"
alias gp="git add -p"
alias ide='ghcid -c "stack ghci --test --ghci-options=-fno-break-on-exception --ghci-options=-fno-break-on-error --ghci-options=-v1 --ghci-options=-ferror-spans --ghci-options=-j"'
path() {echo $PATH | tr : \\n}
alias pbcopy="xclip -sel clip -in"
alias pbpaste="xclip -sel clip -out"

if [[ ! -v ENVIRONMENT_D_RUN ]];
then
  echo source $HOME/.config/environment.d/*.conf
fi
# TODO: Move this up one line after restarting system
source $HOME/.config/environment.d/secrets.conf

export GHCUP_USE_XDG_DIRS=true
[ -f "$XDG_DATA_HOME/ghcup/env" ] && source "$XDG_DATA_HOME/ghcup/env" # ghcup-env
alias ls="ls --color=tty"
bindkey "^I" complete-word

# fnm
if [ -d "$FNM_PATH" ]; then
  eval "`fnm env --use-on-cd --shell zsh --log-level quiet`"
fi
alias nvm=fnm
alias docker-compose='docker compose'

jwt-decode() {
  jq -R 'split(".") | .[0],.[1] | @base64d | fromjson'
}

decode-uri-component() {
  node -e 'const fs = require("fs"); const stdin = fs.readFileSync(0); console.log(decodeURIComponent(stdin.toString()));'
}

bindkey '^[l' down-case-word

docker-mounts() {
  docker inspect -f '{{ .Name }}{{ range .Mounts }}{{ printf "\n\t" }}{{ .Type }} {{ if eq .Type "bind" }}{{ .Source }}{{ end }}{{ .Name }} => {{ .Destination }}{{ end }}' $@
}

source "$ZDOTDIR/penneo.sh"
