{ pkgs, lib, ... }:

{
  imports = [
    ./nvim
  ];

  programs.home-manager.enable = true;

}
