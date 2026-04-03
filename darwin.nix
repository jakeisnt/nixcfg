# darwin.nix --- root module for macOS (nix-darwin) hosts #
# Analogous to default.nix for NixOS, but scoped to what nix-darwin supports.

{ inputs, config, lib, pkgs, ... }:

with lib;
with lib.my;
with inputs; {
  imports =
    [ home-manager.darwinModules.home-manager ]
    ++ [
      # Darwin-specific option wiring (user.packages, home.*, env aliases)
      ./modules/_darwin-options.nix

      # Shell modules verified safe on darwin
      ./modules/shell/nushell.nix   # defines modules.shell.loginInit (needed by fish)
      ./modules/shell/gnupg.nix     # defines modules.shell.gnupg.enable (needed by git)
      ./modules/shell/git.nix
      ./modules/shell/fish.nix

      # Dev modules
      ./modules/dev/node.nix
      ./modules/dev/rust.nix
    ];

  environment.variables = {
    DOTFILES = dotFilesDir;
    NIXPKGS_ALLOW_UNFREE = "1";
  };

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    settings = {
      substituters = [
        "https://cache.nixos.org/"
        "https://nix-community.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };

  environment.systemPackages = with pkgs; [
    coreutils
    wget
    unzip
  ];

  # Required by nix-darwin; bump this when upgrading nix-darwin major versions.
  system.stateVersion = 6;
}
