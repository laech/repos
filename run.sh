#!/usr/bin/env bash

set -e errexit
set -e nounset
set -e pipefail

cd "$(dirname "$0")"

stack run -- --directory /home/lae/projects/self
