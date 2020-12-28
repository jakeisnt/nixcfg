# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../personal.nix
  ];

  networking.hostName = "xps";

  ## Modules
  modules = {
    desktop = {
      sway.enable = true;
      # apps = { rofi.enable = true; };
      browsers = {
        default = "firefox";
        firefox.enable = true;
        # netsurf.enable = true;
        # chrome.enable = true;
      };
      media = {
        daw.enable = true;
        documents.enable = true;
        graphics.enable = true;
        recording.enable = true;
        spotify.enable = true;
      };
      term = {
        default = "st";
        st.enable = true;
      };
      vm = {
        # virtualbox.enable = true;
      };
    };

    media = { mpv.enable = true; };

    messengers = {
      matrix.enable = true;
      discord.enable = true;
      slack.enable = true;
      signal.enable = true;
      email.enable = true;
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
      bluetooth.enable = true;
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
      # tmux.enable = true;
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

  services.xserver.libinput.enable = true;
  services.xserver.xkbOptions = "caps:swapescape";

  users.users.jake.extraGroups = [ "networkmanager" "sway" ];

  # Select internationalisation properties.
  console = { keyMap = "us"; };
}
