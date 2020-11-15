# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ../personal.nix
    ];

  networking.hostName = "xps"; 

  ## Modules
  modules = {
    desktop = {
      bspwm.enable = true;
      apps = {
        rofi.enable = true;
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
        default = "termite";
        termite.enable = true;
      };
      vm = {
        # virtualbox.enable = true;
      };
    };

    media = {
      mpv.enable = true;
    };

    messengers = {
      discord.enable = true;
      slack.enable = true;
      signal.enable = true;
    };
    editors = {
      default = "nvim";
      emacs.enable = true;
      vim.enable = true;
      vscode.enable = true;
    };
    dev = {
      node.enable = true;
      android.enable = true;
      # cc.enable = true;
      # rust.enable = true;
      # lua.enable = true;
      # lua.love2d.enable = true;
    };
    hardware = {
      audio.enable = true;
      fs = {
        enable = true;
        ssd.enable = true;
      };
    };
    shell = {
      direnv.enable = true;
      git.enable = true;
      gnupg.enable = true;
      pass.enable = true;
      tmux.enable = true;
      ranger.enable = true;
      zsh.enable = true;
    };
    services = {
      syncthing.enable = true;
      ssh.enable = true;
    };
    theme.active = "alucard";
  };

  programs.ssh.startAgent = true;
  services.openssh.startWhenNeeded = true;
  networking.networkmanager.enable = true;

  users.users.jake.extraGroups = [ "networkmanager" ];

  # broken touchpad
  services.xserver.libinput.enable = true;
  # duh
  services.xserver.xkbOptions = "caps:swapescape";
  # better hidpi
  services.xserver.monitorSection = ''
    DisplaySize 508 285
  '';

  # Select internationalisation properties.
  console = {
    keyMap = "us";
  };

}

