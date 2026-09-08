# hosts/darwin/mac/default.nix --- personal macOS machine
{ config, pkgs, lib, ... }:

with lib;
with lib.my;
{
  networking.hostName = "mac";

  modules = {
    shell = {
      git.enable = true;
      fish.enable = true;
      nushell.enable = true;
      direnv.enable = true;
    };
    dev = {
      node.enable = true;
      rust.enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    # Google Cloud CLI: provides gcloud, gsutil, and bq.
    google-cloud-sdk

    # Keep the everyday shell tools available even when shell modules are
    # changed independently of this host.
    bat
    eza
    fd
    procs
    tealdeer
    tokei
    zoxide
    ripgrep
    fzf
    jq
  ];
}
