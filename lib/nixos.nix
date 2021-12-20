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
        inputs.sops-nix.nixosModules.sops
        {
          imports = [
            # basic installation framework structure
            (pkgs.path + "/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix")
            # provides initial copy of nixos channel (?)
            (pkgs.path + "/nixos/modules/installer/cd-dvd/channel.nix")
          ];
          nixpkgs.pkgs = pkgs;
          networking.hostName = mkDefault hostname;
          isoImage = {
            volumeID = lib.mkForce hostname;
            isoName = lib.mkForce "${hostname}.iso";
            contents = [
              {
                source = pkgs.writeText "README" "NixOS installer from @jakeisnt's configuration!";
                target = "/README.txt";
              }
            ];
          };
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
