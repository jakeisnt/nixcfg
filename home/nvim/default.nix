{ config, pkgs, lib, ...}:

{
  programs.neovim = {
    package = pkgs.neovim;
    enable = true;

    viAlias = true;
    vimAlias = true;
    withNodeJs = true;
  };

  home.sessionVariables.EDITOR = "nvim";
}
