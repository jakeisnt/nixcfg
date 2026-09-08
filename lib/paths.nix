{ self, lib, pkgs, modules, ... }:

with builtins;
with lib; rec {
  # ...
  dotFilesDir = toString ../.;
  modulesDir = "${dotFilesDir}/modules";
  configDir = "${dotFilesDir}/config";
  binDir = "${dotFilesDir}/bin";
  themesDir = "${modulesDir}/themes";
  # Darwin is installed by arbitrary local accounts; the bootstrap exports
  # DARWIN_USER before evaluating the flake. Keep the Linux account stable.
  username =
    if pkgs.stdenv.isDarwin
    then let name = builtins.getEnv "DARWIN_USER";
         in if name == "" then "jake" else name
    else "jake";
  homeDir = "/home/${username}";

  darwinHomeDir = "/Users/${username}";

  secretsPath = "${dotFilesDir}/secrets.nix";
  secrets =
    if pathExists secretsPath
    then import secretsPath { inherit lib; }
    else {};

  # Equivalent to home-manager's lib.file.mkOutOfStoreSymlink, usable from NixOS modules
  mkOutOfStoreSymlink = path:
    let pathStr = toString path;
    in pkgs.runCommandLocal (baseNameOf pathStr) {} "ln -s ${escapeShellArg pathStr} $out";
}
