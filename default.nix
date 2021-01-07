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
      "dotfiles=${dotFilesDir}"
    ];
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://nix-community.cachix.org"
      "https://nix-cfg.cachix.org"
    ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nix-cfg.cachix.org-1:K5log6+7FTtbqKDRwAhcJvFEdOiavE0aIUWBsYfns4Y="
    ];
    registry = {
      nixos.flake = nixpkgs;
      nixpkgs.flake = nixpkgs-unstable;
    };
    useSandbox = true;
  };
  system.configurationRevision = mkIf (self ? rev) self.rev;
  system.stateVersion = "20.03";

  ## Some reasonable, global defaults
  # This is here to appease 'nix flake check' for generic hosts with no
  # hardware-configuration.nix or fileSystem config.
  fileSystems."/".device = "/dev/disk/by-label/nixos";

  # Use the latest kernel
  boot.kernelPackages = pkgs.linuxPackages_5_9;

  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.configurationLimit = 10;
    systemd-boot.enable = mkDefault true;
  };

  # let's get started!
  environment.systemPackages = with pkgs; [
    # nix tools
    nixfmt

    # linux good to haves
    coreutils
    git
    vim
    wget
    gnumake

    # offer some convenience as needed
    unzip
    cmake
  ];
}
