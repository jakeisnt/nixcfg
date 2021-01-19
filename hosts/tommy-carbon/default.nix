{ config, lib, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "tommy-carbon"; # Define your hostname.
  networking.wireless.enable =
    true; # Enables wireless support via wpa_supplicant.
  networking.wireless.networks = { "8)" = { psk = "not-the-real-password"; }; };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  # so I can mount and read my wiki
  programs.fuse.userAllowOther = true;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  users.users.tommy = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  services = {
    gnome3.gnome-keyring.enable = true;
    upower.enable = true;
    dbus = {
      enable = true;
      socketActivated = true;
      packages = [ pkgs.gnome3.dconf ];
    };
  };

  services.xserver = {
    layout = "us";
    xkbVariant = "colemak";
  };

  environment.systemPackages = with pkgs; [
    feh
    haskellPackages.libmpd
    haskellPackages.xmobar
    libnotify
    lxqt.lxqt-notificationd
    trayer
    xbrightness
    scrot
    xcompmgr
    xorg.xrandr
    xscreensaver
    xsettingsd
    wget
    vim
    firefox
    emacs
    sshfs
    tree
    pavucontrol
    fzf
    discord
    dzen2
    htop
    gimp
    git
    pcmanfm
  ];

  systemd.services.upower.enable = true;
  system.stateVersion = "20.09";
}
