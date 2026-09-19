{ pkgs, ... }:

{
  # Install the repository helper as a real user command. Flake apps are only
  # available through `nix run`; they are not added to a login shell's PATH.
  user.packages = [
    (pkgs.writeShellScriptBin "hey" ''
      exec ${pkgs.bun}/bin/bun ${../../bin/hey.ts} "$@"
    '')
  ];
}
