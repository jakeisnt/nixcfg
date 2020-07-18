{ config, pkgs, lib, ... }:
let
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
  ];

  programs.emacs = {
    enable = true;
    package = doom-emacs;
  };
} // justLinuxAttrs {
  services.emacs.enable = true;
}
