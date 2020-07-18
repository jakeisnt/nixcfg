{ config, pkgs, lib, ... }:
let
  inherit (pkgs) stdenv;
  inherit (stdenv) lib;
  inherit (import ../helpers.nix) justLinuxAttrs;
  doom-emacs = pkgs.callPackage ./nix-doom-emacs {
    doomPrivateDir = ./doom.d;
    extraPackages = epkgs: (with epkgs.melpaPackages; [
      request
    ]);
  };
in
  {
  programs.emacs = {
    enable = true;
    package = doom-emacs;
  };
} // justLinuxAttrs {
  services.emacs.enable = true;
}
