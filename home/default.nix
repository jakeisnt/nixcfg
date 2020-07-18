{ pkgs, lib, ... }:

{
  imports = [
    ./nvim
    ./git.nix
    ./tmux.nix
  ];

  programs.home-manager.enable = true;
}
