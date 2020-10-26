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
      # linux utils
      coreutils
      ripgrep
      wget
      zip
      gnupg
      pinentry
      openssl

      # development help
      sqlite
      valgrind
      direnv

      # nix utils
      niv
      nixfmt

      # config specific
      pulseaudio-ctl

      # photo/video
      darktable
      gimp
      blender
      audacity
    ] ++ justLinux [ syncthing ];

  programs.home-manager.enable = true;
  home.sessionVariables.EDITOR = "emacsclient";
}
