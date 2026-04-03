{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  name = "nixos-config";
}
