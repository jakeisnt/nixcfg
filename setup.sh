#!/usr/bin/env bash
set -eou pipefail

# Thanks to Kyle Sferrazza for this

if [ $# -ne 1 ]; then
  echo "Usage: $1 <hostname>"
  exit 1
fi

HOSTNAME=$1

git submodule init
git submodule update
if [ ! -L configuration.nix ]; then
  cat <<EOF > configuration.nix
{ ... }:
{
  imports = [
    ./hosts/${HOSTNAME}
    ./hosts/${HOSTNAME}/hardware-configuration.nix
    ./config.nix
  ];
}
EOF
fi
