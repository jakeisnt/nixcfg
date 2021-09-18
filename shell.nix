{ pkgs ? import <nixpkgs> {} }:
# https://github.com/hlissner/dotfiles/blob/master/shell.nix
with pkgs;
let
  nixBin = writeShellScriptBin "nix" ''
    ${nixFlakes}/bin/nix --option experimental-features "nix-command flakes" "$@"
  '';

in
mkShell {
  buildInputs = [ git nix-zsh-completions rnix-lsp nixpkgs-fmt ];
  # shellHook = ''
  #   export FLAKE="$(pwd)"
  #   export PATH="$FLAKE/bin:${nixBin}/bin:$PATH"
  # '';
}
