  # Framework laptop!
{ config, pkgs, inputs, lib, ... }:

{
  imports = [./hardware-configuration.nix ../personal.nix];

  networking.hostName = "work";
  services.getty.autologinUser = "jake";

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  time.timeZone = "Europe/Stockholm";

  # automount storage devices
  services.devmon.enable = true;
  user.extraGroups = [
    # unprivileged access to storage devices
    "storage"
    # for scanner
    "scanner"
  ];

  networking = {
    extraHosts = lib.my.secrets.aliasDomains;
    useDHCP = false;
    interfaces.wlp170s0.useDHCP = true;
    networkmanager = {
      enable = true;
      wifi = {
        powersave = false; # no wifi lag
      };
    };
  };

  services.xserver.libinput.enable = true;
  services.openssh.startWhenNeeded = true;
  services.gnome.gnome-keyring.enable = true;

  programs.ssh = {
    startAgent = true;
    forwardX11 = true;
  };

  modules = {
    desktop.sway = {
      enable = true;
      fancy = true;
      scale = 1.25;
    };
    browsers = {
      default = "firefox";
      firefox.enable = true;
      chrome.enable = true;
    };
    term = {
      default = "alacritty";
      alacritty.enable = true;
      foot.enable = true;
    };
    editors = {
      default = "emacsclient -c";
      vim.enable = true;
      emacs = {
        enable = true;
        daemon = true;
      };
    };
    hardware = {
      audio.enable = true;
      # bluetooth.enable = true;
    };
    # messengers.email.enable = true;
    media = {
      ncmpcpp.enable = true;
      recording.enable = true;
      # TODO:  These options require python2.
      # graphics.enable = true;
      # graphics.photo.enable = true;
    };
    shell = {
      git.enable = true;
      file.enable = true;
      nushell.enable = true;
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
  };
}
