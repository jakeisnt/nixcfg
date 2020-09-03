{ config, lib, pkgs, ... }:

{
  programs.starship = {
    enable = true;
    settings = { nix_shell = { use_name = true; }; };
  };
}
