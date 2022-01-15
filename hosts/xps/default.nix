# My Dell XPS 9370: 4K monitor, malfunctioning screen and trackpad
{ config, pkgs, lib, ... }:

with lib;
with lib.my;
{
  imports = [ ./hardware-configuration.nix ../personal.nix ];

  networking.hostName = "xps";

  users.users.jake.packages = with pkgs; [ thunderbird openssl ];
  services.fwupd.enable = true;
  services.xserver.libinput.enable = true;

  programs.ssh = {
    startAgent = true;
    forwardX11 = true;
  };

  services.openssh.startWhenNeeded = true;
  services.getty.autologinUser = "jake";

  networking = {
    networkmanager = {
      enable = true;
      wifi = {
        powersave = false; # wifi beast mode
      };
    };
  };

  modules = {
    desktop.sway = {
      enable = true;
      fancy = true;
      disable-touch = true;
    };
    browsers = {
      default = "firefox";
      firefox.enable = true;
    };
    media = {
      documents.enable = true;
      graphics = {
        vector.enable = false;
        sprites.enable = false;
        enable = true;
      };
      recording.enable = true;
      spotify.enable = true;
    };
    term = {
      default = "alacritty";
      alacritty.enable = true;
    };
    media = {
      mpv.enable = true;
      # ncmpcpp.enable = true; needs fixing, not used
    };
    messengers = {
      rss.enable = true;
      matrix.enable = true;
      signal.enable = true;
      # email.enable = true;
      weechat.enable = true;
    };
    editors = {
      default = "emacsclient -c";
      emacs = {
        enable = true;
        daemon = true;
      };
      vim.enable = false;
      vscode.enable = true;
    };

    # TODO consider deleting
    dev = {
      node.enable = true;
      cc.enable = false;
      rust.enable = false; # should be project local
    };

    hardware = {
      remarkable.enable = true;
      extraHosts = {
        enable = true;
        allowSocial = true;
      };
      audio.enable = true;
      bluetooth.enable = true;
      scanner.enable = true;
      printer.enable = true;
      fs = {
        enable = true;
        ssd.enable = true;
      };
    };
    shell = {
      git.enable = true;
      gnupg = {
        enable = true;
        gui = true;
        cacheTTL = 60480000;
      };
      direnv = {
        enable = true;
        preventGC = true;
      };
      lf.enable = true;
      fish.enable = true;
    };
    services = {
      docker.enable = true;
      syncthing.enable = true;
      ssh.enable = true;
      backup.enable = true;
      dnsmasq.enable = true;
    };
  };
}
