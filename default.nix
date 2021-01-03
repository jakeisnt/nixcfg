{ inputs, config, lib, pkgs, ... }:

with lib;
with lib.my;
with inputs; {
  imports =
    # I use home-manager to deploy files to $HOME; little else
    [
      home-manager.nixosModules.home-manager
      simple-nixos-mailserver.nixosModules.mailserver
    ]
    # All my personal modules
    ++ (mapModulesRec' (toString ./modules) import);

  # Common config for all nixos machines to ensure the flake operates soundly
  environment.variables.DOTFILES = dotFilesDir;

  # Configure nix and nixpkgs
  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";
    nixPath = [
      "nixpkgs=${nixpkgs}"
      "nixpkgs-unstable=${nixpkgs-unstable}"
      "nixpkgs-overlays=${dotFilesDir}/overlays"
      "home-manager=${home-manager}"
      "simple-nixos-mailserver=${simple-nixos-mailserver}"
      "dotfiles=${dotFilesDir}"
    ];
    binaryCaches =
      [ "https://cache.nixos.org/" "https://nix-community.cachix.org" ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
    registry = {
      nixos.flake = nixpkgs;
      nixpkgs.flake = nixpkgs-unstable;
    };
    useSandbox = true;
  };
  system.configurationRevision = mkIf (self ? rev) self.rev;
  system.stateVersion = "20.09";

  ## Some reasonable, global defaults
  # This is here to appease 'nix flake check' for generic hosts with no
  # hardware-configuration.nix or fileSystem config.
# TODO this caused problems on the server
  fileSystems."/".device = "/dev/vda1";

  # Use the latest kernel
  boot.kernelPackages = pkgs.linuxPackages_5_9;

  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.configurationLimit = 10;
    systemd-boot.enable = mkDefault true;
  };

  # enable lorri for shell.nix!
  services.lorri.enable = true;

  # Just the bear necessities...
  environment.systemPackages = with pkgs; [
    # nix tools
    cached-nix-shell
    niv
    nixfmt

    # linux good to haves
    coreutils
    git
    vim
    wget
    gnumake

    # unliked, but offer extra convenience
    unzip
    cmake
  ];
}
