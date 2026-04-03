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
    };
    dev = {
      node.enable = true;
      rust.enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    ripgrep
    fzf
    jq
  ];
}
