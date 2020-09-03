{ config, lib, pkgs, ... }:

{
  programs.starship = {
    enable = false;
    settings = { nix_shell = { use_name = true; }; };
  };
}
