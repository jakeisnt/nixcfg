{ self, lib, pkgs, modules, ... }:

with builtins;
with lib; rec {
  # ...
  dotFilesDir = toString ../.;
  modulesDir = "${dotFilesDir}/modules";
  configDir = "${dotFilesDir}/config";
  binDir = "${dotFilesDir}/bin";
  themesDir = "${modulesDir}/themes";
  username = "jake";
  homeDir = "/home/${username}";

  darwinHomeDir = "/Users/${username}";

  secrets = import ./secrets.nix { inherit lib; };

  # Equivalent to home-manager's lib.file.mkOutOfStoreSymlink, usable from NixOS modules
  mkOutOfStoreSymlink = path:
    let pathStr = toString path;
    in pkgs.runCommandLocal (baseNameOf pathStr) {} "ln -s ${escapeShellArg pathStr} $out";
}
