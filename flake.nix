# flake.nix --- the heart of my dotfiles
#
# Author:  Henrik Lissner <henrik@lissner.net>
# URL:     https://github.com/hlissner/dotfiles
# License: MIT
#
# Welcome to ground zero. Where the whole flake gets set up and all its modules
# are loaded.

{
  description = "A grossly incandescent nixos config.";

  inputs = {
    # Core dependencies.
    # Two inputs so I can track them separately at different rates.
    nixpkgs.url = "nixpkgs/master";
    nixpkgs-unstable.url = "nixpkgs/master";


    home-manager.url = "github:rycee/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nur.url = "github:nix-community/NUR/master";

    # Extras
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixos-hardware.url = "github:nixos/nixos-hardware";

    simple-nixos-mailserver = {
      url =
      "gitlab:simple-nixos-mailserver/nixos-mailserver";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    spicetify-nix = {
      url = "github:jakeisnt/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-remarkable = {
      url = "github:siraben/nix-remarkable";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    doom-emacs = {
      url = "github:hlissner/doom-emacs/develop";
      flake = false;
    };

    isntweb-home.url = "github:isntweb/home";

    # neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    # broken too frequently for me to be able to live on the bleeding edge...
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, spicetify-nix, doom-emacs, isntweb-home, ... }:
    let
      inherit (lib) attrValues;
      inherit (lib.my) mapModules mapModulesRec mapHosts;

      system = "x86_64-linux";

      mkPkgs = pkgs: extraOverlays:
        import pkgs {
          inherit system;
          config = {
            android_sdk.accept_license = true;
            allowUnfree = true;
          };
          overlays = extraOverlays ++ (attrValues self.overlays);
        };

      pkgs = mkPkgs nixpkgs [
        self.overlay
        (self: super: { doomEmacsRevision = doom-emacs.rev; })
      ];
      uPkgs = mkPkgs nixpkgs-unstable [];

      lib = nixpkgs.lib.extend (
        self: super: {
          my =
            import ./lib {
            inherit pkgs inputs;
            lib = self;
          };
        }
      );
    in
      {
        overlay = final: prev: {
          unstable = uPkgs;
          my = self.packages."${system}";
          extras = { inherit spicetify-nix; };
        };

        overlays = mapModules ./overlays import;

        packages."${system}" = mapModules ./packages (p: pkgs.callPackage p {});

        nixosModules = {
          dotfiles = import ./.;
        } // mapModulesRec ./modules import;

        nixosConfigurations = mapHosts ./hosts { inherit system; };

        devShell."${system}" = pkgs.mkShell {
          name = "nixos-config";
        };

        templates = {
          full = {
            path = ./.;
            description = "A grossly incandescent nixos config";
          };
          minimal = {
            path = ./templates/minimal;
            description = "A grossly incandescent and minimal nixos config";
          };
        };

        defaultTemplate = self.templates.minimal;

        defaultApp."${system}" = {
          type = "app";
          program = ./bin/hey;
        };
      };
}
