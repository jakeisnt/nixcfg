{ config, lib, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  networking.hostName = "tommy-carbon"; # Define your hostname.
  # TODOs:
  # put password in lib/secrets.nix
  # Add 'tommy' to 'lib/secrets.nix'

  modules = {
    desktop = {
      xmonad.enable = true;
      browsers = {
        default = "firefox";
        firefox.enable = true;
      };
      media = {
        documents.enable = true;
        graphics.enable = true;
        spotify.enable = true;
      };
      term = {
        default = "alacritty";
        alacritty.enable = true;
      };
    };
    messengers = { discord.enable = true; };
    editors = {
      default = "emacs";
      vim.enable = true;
      emacs = {
        enable = true;
        daemon = true;
      };
    };
    hardware = {
      audio.enable = true;
      bluetooth.enable = true;
      fs = {
        enable = true;
        ssd.enable = true;
      };
    };
    shell = { git.enable = true; };
    services = { ssh.enable = true; };
  };

  networking.wireless.enable = true;
  networking.wireless.networks = { "8)" = { psk = "not-the-real-password"; }; };

  # so I can mount and read my wiki
  programs.fuse.userAllowOther = true;

  services = {
    gnome3.gnome-keyring.enable = true;
    upower.enable = true;
    dbus = {
      enable = true;
      packages = [ pkgs.gnome3.dconf ];
    };
  };

  services.xserver.xkbVariant = "colemak";

  environment.systemPackages = with pkgs; [
    feh
    haskellPackages.libmpd
    haskellPackages.xmobar
    lxqt.lxqt-notificationd
    trayer
    xbrightness
    scrot
    xcompmgr
    xorg.xrandr
    xscreensaver
    xsettingsd
    wget
    sshfs
    tree
    pavucontrol
    fzf
    dzen2
    htop
    gimp
    pcmanfm
  ];

  systemd.services.upower.enable = true;
  system.stateVersion = "20.09";
}
