{ pkgs, sops-nix, ... }:
# https://github.com/hlissner/dotfiles/blob/master/shell.nix
with pkgs;
let
  # nixBin = writeShellScriptBin "nix" ''
  #   ${nixFlakes}/bin/nix --option experimental-features "nix-command flakes" "$@"
  #   '';
  # sops-nix = builtins.fetchTarball {
  #   url = "https://github.com/Mic92/sops-nix/archive/master.tar.gz";
  #   sha256 = "0ga528x3sci1y10vd8m5l2wnsl7kkf14whwp7dkwcbkvm34n88yx";
  # };
  sops-bins = pkgs.callPackage sops-nix {};
in
mkShell {
  sopsPGPKeyDirs = [ ./keys ];
  # sopsCreateGPGHome = true;
  buildInputs = [ git nix-zsh-completions rnix-lsp nixpkgs-fmt ];

  nativeBuildInputs = with sops-bins; [
    sops
    sops-import-keys-hook
    sops-init-gpg-key
  ];
  # shellHook = ''
  #   export FLAKE="$(pwd)"
  #   export PATH="$FLAKE/bin:${nixBin}/bin:$PATH"
  # '';
}
