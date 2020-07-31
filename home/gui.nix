{ pkgs, lib, ... }:
let inherit (import ./helpers.nix) justLinux justLinuxAttrs;
in {
  imports = [
    ./nvim
    ./emacs
    ./firefox.nix
    ./git.nix
    ./tmux.nix
    ./shells.nix
    ./redshift.nix
    ./udiskie.nix
    ./picom.nix
    ./lorri.nix
    ./xdg.nix
  ];

  services.xcape = {
    enable = true;
    mapExpression = { Hyper_L = "Escape"; };
  };

  home.packages = with pkgs;
    [
      ranger
      ripgrep
      valgrind
      wget
      coreutils
      sqlite
      direnv
      niv
      nixfmt
      pulseaudio-ctl
    ] ++ justLinux [ fzf syncthing ];

  programs.home-manager.enable = true;
  home.sessionVariables.EDITOR = "emacs";
}
