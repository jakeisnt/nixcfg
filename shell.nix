{ pkgs, sops-nix, ... }:
# https://github.com/hlissner/dotfiles/blob/master/shell.nix
with pkgs;
let
  sops-bins = pkgs.callPackage sops-nix {};
in
mkShell {
  sopsPGPKeyDirs = [ ./keys ];
  buildInputs = [ git nix-zsh-completions rnix-lsp nixpkgs-fmt ];
  nativeBuildInputs = with sops-bins; [
    sops
    sops-import-keys-hook
    sops-init-gpg-key
  ];
  shellHook = ''
    export FLAKE="$(pwd)"
  '';
}
