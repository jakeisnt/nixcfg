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
}
