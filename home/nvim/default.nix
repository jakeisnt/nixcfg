{ config, pkgs, lib, ...}:

{
  imports = [
    ./plugins
  ];

  programs.neovim = {
    package = pkgs.neovim;
    enable = true;

    viAlias = true;
    vimAlias = true;
    withNodeJs = true;
  };

  home.sessionVariables.EDITOR = "nvim";
}
