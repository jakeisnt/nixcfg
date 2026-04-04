# flake.nix --- the heart of my dotfiles #
# Author:  Henrik Lissner <henrik@lissner.net> ++ Jake Chvatal <jakechvatal@gmail.com>
# URL:     https://github.com/jakeisnt/nixcfg
# License: MIT

{
  description = "jakeisnt's nix configuration";

  inputs = {
    # Two inputs so I can track them separately at different rates.
    nixpkgs.url = "nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "nixpkgs/master";

    nix-claude-code = {
      url = "github:ryoppippi/nix-claude-code";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager-unstable = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    nur = {
      url = "github:nix-community/NUR/master";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    spicetify-nix = {
      url = "github:jakeisnt/spicetify-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Pin Doom Emacs version
    doom-emacs = {
      url = "github:doomemacs/doomemacs/master";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, nix-claude-code, spicetify-nix, doom-emacs, ... }:
    let
      inherit (lib) attrValues;
      inherit (lib.my) mapModules mapModulesRec mapHosts mapDarwinHosts;

      system = "x86_64-linux";
      darwinSystem = "aarch64-darwin";

      mkPkgs = pkgs: extraOverlays:
        import pkgs {
          inherit system;
          config = {
            android_sdk.accept_license = true;
            allowUnfree = true;
          };
          overlays = extraOverlays ++ (attrValues self.overlays) ++ [
            nix-claude-code.overlays.default
          ];
        };

      uPkgs = mkPkgs nixpkgs-unstable [
        self.overlay
        (self: super: { doomEmacsRevision = doom-emacs.rev; })
      ];

      pkgs = mkPkgs nixpkgs [
        self.overlay
        (self: super: {
          doomEmacsRevision = doom-emacs.rev;
          unstable = uPkgs;
        })
      ];


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

      darwinConfigurations = mapDarwinHosts ./hosts/darwin { system = darwinSystem; };

      devShell."${system}" = pkgs.mkShell {
        name = "nixos-config";
      };

      devShell."aarch64-darwin" = (import nixpkgs { system = "aarch64-darwin"; }).mkShell {
        name = "nixos-config";
      };

      templates = {
        full = {
          path = ./.;
          description = "A grossly incandescent nixos config";
        };
        flake = {
          path = ./templates/flake;
          description = "A simple Nix flake starter project.";
        };
      };

      defaultTemplate = self.templates.flake;

      defaultApp."${system}" = {
        type = "app";
        program = ./bin/hey;
      };
    };
}
