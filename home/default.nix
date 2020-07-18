{ pkgs, lib, ... }:

{
  imports = [
    ./nvim
    ./emacs
    ./git.nix
    ./tmux.nix
  ];

  programs.home-manager.enable = true;
}
