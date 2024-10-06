{ config, options, lib, pkgs, uPkgs, inputs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors.vim;
  launchNvim = "nvim -u ~/.config/nvim/init.lua";
in
{
  options.modules.editors.vim = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    programs.neovim = {
      package = pkgs.unstable.neovim-unwrapped; # switch from (neovim-nightly)
      enable = true;
      defaultEditor = false; # this is configured by me elsewhere
    };

    user.packages = with pkgs; [
        sqlite          # for coq nvim completion
        universal-ctags # for coq nvim completion
        python310       # for coq setup
        python310Packages.virtualenv
        # lua lsp for vim config
        sumneko-lua-language-server
        # for nvim
        gcc
        # for copilot (and others?)
        nodejs
    ];

    modules.editors.tree-sitter.enable = true;

    # env.VIMINIT = "let \\$MYVIMRC='\\$XDG_CONFIG_HOME/nvim/init.lua' | source \\$MYVIMRC";

    environment.shellAliases = {
      nvim = launchNvim;
      vim = launchNvim;
      vi = launchNvim;
      v = launchNvim;
    };
  };
}
