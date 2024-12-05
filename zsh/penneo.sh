alias awws='echo $@ | xargs -n1 aww run'

openvpn-connect() {
  CREDENTIALS=${1:-$HOME/.config/openvpn/credentials/fhi@penneo.com}
  openvpn --config product_fhi@penneo.com_penneo-ops.ovpn --auth-user-pass --auth-retry interact --auth-user-pass $CREDENTIALS
}

export PENNEO="$HOME/git/penneo"
export PATH="$XDG_DATA_HOME/jenv/bin:$PATH"
eval "$(jenv init -)"

sign-links() {
  tail -f -n+1 $PENNEO/devenv/logs/worker-sign/mock-emails.log| grep -oE 'https://dev.penneo.com.*sign[^"]*' --color=never
}

a-b() {
  echo hej
}

# Copied (most) things from $PENNEO/devenv/.envrc:
export COMPOSE_PATH_SEPARATOR=,
export COMPOSE_FILE=compose.base.yml,compose.storage.yml,compose.gateway.yml,compose.sign.yml,compose.auth.yml,compose.pdf.yml,compose.penneo-ca.yml,compose.media.yml,compose.eid.yml,compose.form.yml
export COMPOSE_PROFILES=cron,worker

# DNS MasQ - customized port for DNS
export DNS_PORT=54

# Kafka (RedPanda)
export KCAT_CONFIG=.kcat.conf
export PDF_SERVICE_CONFIG_PATH="$PENNEO/devenv/sign/services/parameters_dev.yml"
