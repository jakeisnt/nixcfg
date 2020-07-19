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
  home.packages = with pkgs; [
    emacs-all-the-icons-fonts
    fira-code-symbols

    (makeDesktopItem {
      name = "org-protocol";
      exec = "emacsclient %u";
      comment = "Org Protocol";
      desktopName = "org-protocol";
      type = "Application";
      mimeType = "x-scheme-handler/org-protocol";
    })
  ];

  programs.emacs = {
    enable = true;
    package = doom-emacs;
  };





} // justLinuxAttrs {
  services.emacs.enable = true;
}
