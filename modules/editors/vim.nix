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
      configure = {
	customRC = ''
	  lua require('init')
	'';
      };
    };

    user.packages = with pkgs; [
    	vimPlugins.packer-nvim
        # coq nvim completion
        sqlite
        universal-ctags # for coq nvim completion
        # for coq setup
        python310
        python310Packages.virtualenv

        file # for lf?

        # lua lsp for vim config
        sumneko-lua-language-server
    ];

    modules.editors.tree-sitter.enable = true;

    # source '/home/jake/.config/nvim/theme.vim'

    env.VIMINIT = "let \\$MYVIMRC='\\$XDG_CONFIG_HOME/nvim/init.vim' | source \\$MYVIMRC";

    environment.shellAliases = {
      vim = "nvim";
      vi = "nvim";
      v = "nvim";
    };
  };
}
