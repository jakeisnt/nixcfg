# framework laptop
{ config, pkgs, ... }:
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../personal.nix # TODO i kept screwing up bc i forgot this (and networking wheel perms!
    ];

  networking.hostName = "work"; # Define your hostname.
  modules = {
    desktop.sway = {
      enable = true;
      fancy = true;
    };
    browsers = {
      default = "firefox";
      firefox.enable = true;
    };
    term = {
      default = "alacritty";
      alacritty.enable = true;
    };
    editors = {
      default = "emacsclient -c";
      emacs = {
        enable = true;
        daemon = true;
      };
    };
    hardware = {
      audio.enable = true;
      bluetooth.enable = true;
    };

    shell = {
      git.enable = true;
      lf.enable = true;
      zsh.enable = true;
      gnupg = {
        enable = true;
        gui = true;
        cacheTTL = 60480000;
      };
      direnv = {
        enable = true;
        preventGC = true;
      };
    };
    theme.active = "nordic";

  };

  services.getty.autologinUser = "jake";

  services.fprintd.enable = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.useDHCP = false;
  networking.interfaces.wlp170s0.useDHCP = true;

  users.users.jake = {
    isNormalUser = true;
    password = "jake";
    extraGroups = [ "wheel" "networkmanager" "network"]; # Enable ‘sudo’ for the user.
  };

  networking.networkmanager.enable = true;
  services.xserver.libinput.enable = true;
  services.openssh.startWhenNeeded = true;
  programs.ssh = {
    startAgent = true;
    forwardX11 = true;
  };
}
