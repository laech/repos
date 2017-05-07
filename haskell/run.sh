#!/usr/bin/env bash

set -e errexit

cd "$(dirname $0)"

echo 'Running Stack build...'

stack build

stack exec fetch-my-bitbucket-projects ../config.json
