#!/usr/bin/env bash

set -e errexit

cd "$(dirname $0)"

echo 'Running Stack build...'

stack build

GIT_SSH_COMMAND="ssh -o UserKnownHostsFile=""$(pwd)""/known_hosts" \
stack exec projects config.json
