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

  # Use the systemd-boot EFI boot loader.
  # boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  # networking.useDHCP = false;
  # networking.interfaces.wlp2s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  ## Modules
  modules = {
    desktop = {
      bspwm.enable = true;
      apps = {
        discord.enable = true;
        rofi.enable = true;
      };
      browsers = {
        default = "firefox";
        firefox.enable = true;
      };
      media = {
        # daw.enable = true;
        documents.enable = true;
        graphics.enable = true;
        mpv.enable = true;
        recording.enable = true;
        spotify.enable = true;
      };
      term = {
        default = "xst";
        st.enable = true;
      };
      vm = {
        # virtualbox.enable = true;
      };
    };
    editors = {
      default = "nvim";
      emacs.enable = true;
      vim.enable = true;
    };
    dev = {
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
      # weechat.enable = true;
      pass.enable = true;
      tmux.enable = true;
      # ranger.enable = true;
      zsh.enable = true;
    };
    hardware = {

    };
    services = {
      # syncthing.enable = true;
      ssh.enable = true;
    };
    theme.active = "alucard";
  };

  programs.ssh.startAgent = true;
  services.openssh.startWhenNeeded = true;
  networking.networkmanager.enable = true;


  # broken touchpad
  services.xserver.libinput.enable = true;
  # duh
  services.xserver.xkbOptions = "caps:swapescape";
  # better hidpi
  services.xserver.monitorSection = ''
    DisplaySize 508 285
  '';
}

