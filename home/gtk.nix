{ config, lib, pkgs, ... }:

{
  gtk = {
      enable = true;
      theme = pkgs.nordic;
      iconTheme = pkgs.paper-icon-theme;
  };
}
