{ self, lib, ... }:

with builtins;
with lib; rec {
  # ...
  dotFilesDir = toString ../.;
  modulesDir = "${dotFilesDir}/modules";
  configDir = "${dotFilesDir}/config";
  secretsDir = "${dotFilesDir}/secrets";
  binDir = "${dotFilesDir}/bin";
  themesDir = "${modulesDir}/themes";
  homeDir = "/home/${
      let name = getEnv "USERNAME";
      in if elem name [ "jake" "root" ] then "jake" else name
    }";
}
