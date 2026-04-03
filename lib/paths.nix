{ self, lib, modules, ... }:

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
}
