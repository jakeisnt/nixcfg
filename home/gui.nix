{ pkgs, lib, ... }:
let
  inherit (import ./helpers.nix) justLinux justLinuxAttrs;
in
{
  imports = [
    ./nvim
    ./emacs
    ./firefox.nix
    ./git.nix
    ./tmux.nix
    ./shells.nix
  ];

  services.xcape = {
    enable = true;
    mapExpression = {
      Hyper_L = "Escape";
    };
  };

  home.packages = with pkgs; [
    ranger
    ripgrep
    valgrind
    wget
    coreutils
    sqlite
  ] ++ justLinux [
    fzf
    syncthing
  ];

  programs.home-manager.enable = true;
}
