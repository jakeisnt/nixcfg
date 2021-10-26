{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors.vim;
in
{
  options.modules.editors.vim = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    programs.neovim = {
      package = pkgs.neovim-unwrapped; # switch from (neovim-nightly)
      enable = true;
      defaultEditor = false; # this is configured by me elsewhere
    };

    # environment.shellAliases = {
    #   vim = "nvim";
    #   vi = "nvim";
    #   v = "nvim";
    # };
  };
}
