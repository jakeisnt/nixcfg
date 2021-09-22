{ pkgs ? import <nixpkgs> {} }:
# https://github.com/hlissner/dotfiles/blob/master/shell.nix
with pkgs;
let
  nixBin = writeShellScriptBin "nix" ''
    ${nixFlakes}/bin/nix --option experimental-features "nix-command flakes" "$@"
    '';
  sops-nix = builtins.fetchTarball {
    url = "https://github.com/Mic92/sops-nix/archive/master.tar.gz";
    sha256 = "0ga528x3sci1y10vd8m5l2wnsl7kkf14whwp7dkwcbkvm34n88yx";
  };
in
mkShell {
  sopsPGPKeyDirs = [ ./keys ];
  buildInputs = [ git nix-zsh-completions rnix-lsp nixpkgs-fmt ];

  nativeBuildInputs = [
    (pkgs.callPackage sops-nix {}).sops-import-keys-hook
    sops
  ];
  # shellHook = ''
  #   export FLAKE="$(pwd)"
  #   export PATH="$FLAKE/bin:${nixBin}/bin:$PATH"
  # '';
}
