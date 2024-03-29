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
  homeDir = "/home/${
  let
    name = getEnv "USERNAME";
  in
    if elem name [ username "root" ] then username
    else
      name
  }";

  secrets = import ./secrets.nix { inherit lib; };
}
