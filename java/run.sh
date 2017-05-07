#!/usr/bin/env bash

set -o errexit

cd "$(dirname $0)"

echo 'Running Maven build...'
mvn package

java -jar target/fetch-my-bitbucket-projects.jar ../config.json
