{ pkgs, lib, ... }:
let inherit (import ./helpers.nix) justLinux justLinuxAttrs;
in {
  imports = [
    ./emacs
    ./firefox.nix
    ./git.nix
    ./shells.nix
    # ./starship.nix
    # ./redshift.nix
    ./udiskie.nix
    ./picom.nix
    ./lorri.nix
    ./xdg.nix
    ./fzf.nix
    ./pass.nix
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
      darktable
      gimp
    ] ++ justLinux [ syncthing ];

  programs.home-manager.enable = true;
  home.sessionVariables.EDITOR = "emacsclient %u";
}
