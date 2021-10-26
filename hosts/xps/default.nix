{ config, pkgs, lib, ... }:

with lib;
with lib.my;
{
  imports = [ ./hardware-configuration.nix ../personal.nix ];

  networking.hostName = "xps";

  modules = {
    vm.virtualbox = {
      enable = true;
    };
    desktop.sway = {
      enable = true;
      fancy = true;
    };
    browsers = {
      default = "firefox";
      firefox.enable = true;
    };
    media = {
      daw.enable = true;
      documents.enable = true;
      graphics.enable = true;
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
      vim.enable = true;
      vscode.enable = true;
    };
    dev = {
      node.enable = true;
      cc.enable = true;
      rust.enable = true;
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
      zsh.enable = true;
    };
    services = {
      syncthing.enable = true;
      ssh.enable = true;
      backup.enable = true;
      dnsmasq.enable = true;
    };
    theme.active = "nordic";
  };

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

  sops.secrets.password = {};

  users.users.jake.extraGroups = [ "networkmanager" ];
  users.users.jake.packages = with pkgs; [ thunderbird openssl z3 ]; # TODO may not be needed

  services.fwupd.enable = true;
  services.xserver.libinput.enable = true;

}
