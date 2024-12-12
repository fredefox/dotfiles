alias awws='echo $@ | xargs -n1 aww run'

openvpn-connect() {
  OPENVPN=${XDG_CONFIG_HOME:-$HOME/.config}/openvpn
  CONFIG=$OPENVPN/profiles/product_fhi@penneo.com_penneo-ops.ovpn
  CREDENTIALS=$OPENVPN/credentials/fhi@penneo.com
  openvpn \
    --config $CONFIG \
    --auth-user-pass \
    --auth-retry interact \
    --auth-user-pass $CREDENTIALS
}

export PENNEO="$HOME/git/penneo"
export PATH="$XDG_DATA_HOME/jenv/bin:$PATH"
eval "$(jenv init -)"

sign-links() {
  tail -f -n+1 \
       $PENNEO/devenv/logs/worker-sign/mock-emails.log \
    | grep -oE 'https://dev.penneo.com.*sign[^"]*' --color=never --line-buffered \
    | uniq
}

export DEVENV=$PENNEO/devenv
