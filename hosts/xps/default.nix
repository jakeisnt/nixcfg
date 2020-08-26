{ config, lib, pkgs, ... }:

{
  imports = [ <nixos-hardware/dell/xps/13-9370> ./hardware-configuration.nix ];

  home-manager.users.jake = import ../../home/gui.nix;
  services.xserver = {
    enable = true;
    layout = "us";
    libinput.enable = true; # enable touchpad
    xkbOptions = "caps:swapescape";
    displayManager.defaultSession = "none+exwm";
    windowManager.session = lib.singleton {
      name = "exwm";
      start = ''
        emacs --daemon -f exwm-enable
        emacsclient -c
      '';
    };

    monitorSection = ''
      DisplaySize 508 285
    '';
  };

  # pulseaudio hardware and software drivers
  hardware.pulseaudio = {
    enable = true;
    # ensure that bluetooth support is included
    package = pkgs.pulseaudioFull;
    # enable extra bluetooth codecs
    extraModules = [ pkgs.pulseaudio-modules-bt ];
    support32Bit = true;
    extraConfig = ''
      load-module module-switch-on-connect
    '';
  };

  # bluetooth hardware and software utilities
  hardware.bluetooth = {
    enable = true;
    extraConfig = ''
      [General]
      Enable=Source,Sink,Media,Socket
    '';
  };
  services.blueman.enable = true;

  boot.plymouth.enable = true;

  # sync files with other devices (requires manual setup)
  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    systemService = true;
    user = "jake";
    group = "wheel";
    dataDir = "/home/jake";
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  powerManagement.enable = true;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp2s0.useDHCP = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = { keyMap = "us"; };
  services.localtime.enable = true;
  programs.light.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;
  # services.printing.drivers = [ ];

  networking.hostName = "xps";
  networking.networkmanager.enable = true;
  networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}
