{ inputs, lib, pkgs, ... }:

with lib;
with lib.my;
let system = "x86_64-linux";
in {
  mkHost = path: attrs @ { system ? system, ... }:
    let hostname = (removeSuffix ".nix" (baseNameOf path)); in
    nixosSystem {
      inherit system;
      specialArgs = { inherit lib inputs; };
      modules = [
        {
          nixpkgs.pkgs = pkgs;
          networking.hostName = mkDefault hostname;
        }
        (filterAttrs (n: v: !elem n [ "system" ]) attrs)
        ../.
        (import path)
      ];
    };

  mapHosts = dir: attrs @ { system ? system, ... }:
    mapModules dir
      (hostPath: mkHost hostPath attrs);

  mkDarwinHost = path: attrs @ { system ? "aarch64-darwin", ... }:
    let
      hostname = (removeSuffix ".nix" (baseNameOf path));
      darwinPkgs = import inputs.nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          (final: prev: {
            unstable = import inputs.nixpkgs-unstable {
              inherit system;
              config.allowUnfree = true;
            };
            my = {};
          })
        ];
      };
    in
    inputs.nix-darwin.lib.darwinSystem {
      inherit system;
      specialArgs = { inherit lib inputs; };
      modules = [
        { nixpkgs.pkgs = darwinPkgs;
          networking.hostName = mkDefault hostname; }
        (filterAttrs (n: v: !elem n [ "system" ]) attrs)
        ../darwin.nix
        (import path)
      ];
    };

  mapDarwinHosts = dir: attrs @ { system ? "aarch64-darwin", ... }:
    mapModules dir
      (hostPath: mkDarwinHost hostPath attrs);
}
