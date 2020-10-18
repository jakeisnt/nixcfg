{ pkgs, lib, ... }:
let inherit (import ./helpers.nix) justLinux justLinuxAttrs;
in {
  imports = [
    ./emacs
    ./firefox.nix
    ./git.nix
    ./shells.nix
    # ./redshift.nix
    ./udiskie.nix
    ./picom.nix
    ./lorri.nix
    ./xdg.nix
    ./fzf.nix
    ./xresources.nix
    ./pass.nix
  ];

  services.xcape = {
    enable = true;
    mapExpression = { Hyper_L = "Escape"; };
  };

  home.packages = with pkgs;
    [
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
      gnupg
      pinentry
      blender
      zip
      niv
      docker
    ] ++ justLinux [ syncthing ];

  programs.home-manager.enable = true;
  home.sessionVariables.EDITOR = "emacsclient";
}
