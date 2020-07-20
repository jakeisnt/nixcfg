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
    ./x.nix
    ./gtk.nix
    ./redshift.nix
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
    direnv
  ] ++ justLinux [
    fzf
    syncthing
  ];

  services.lorri.enable = true;

  programs.home-manager.enable = true;
}
