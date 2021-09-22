{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors.vim;
in
{
  options.modules.editors.vim = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # nixpkgs.overlays = with inputs; [ neovim-nightly-overlay.overlay ];
    user.packages = with pkgs; [
      editorconfig-core-c
      tree-sitter
      gcc
      # for nvim config file
      sumneko-lua-language-server
      luaformatter

      nodePackages.prettier
      nodePackages.eslint_d
      nodePackages.diagnostic-languageserver
    ];

    programs.neovim = {
      package = pkgs.neovim-unwrapped; # switch from (neovim-nightly)
      enable = true;
      defaultEditor = false; # this is configured by me elsewhere

      configure = {
        customRC = ''
          lua require('init')
          colorscheme nord
        '';

        packages.myPlugins = with pkgs.vimPlugins; {

          start = [
            vim-polyglot
            vim-commentary
            nvim-treesitter
            nvim-treesitter-textobjects
            nvim-lspconfig
            completion-nvim
            nvim-compe
            telescope-nvim
            popup-nvim
            plenary-nvim
            nord-nvim
            which-key-nvim
            neogit
            diffview-nvim
            gitsigns-nvim
            auto-pairs
            direnv-vim
            snippets-nvim
            lualine-nvim
            nvim-web-devicons

            nvim-autopairs # trial run
            nvim-colorizer-lua
            lush-nvim
            diagnostic-nvim
            lspsaga-nvim
            rust-tools-nvim

            vim-racket
            conjure

            lightspeed-nvim
          ];
        };
      };
    };

    env = {
      VIMINIT =
        "let \\$MYVIMRC='\\$XDG_CONFIG_HOME/nvim/init.vim' | source \\$MYVIMRC";
    };

    environment.shellAliases = {
      vim = "nvim";
      vi = "nvim";
      v = "nvim";
    };

    home.configFile = {
      "nvim" = {
        source = "${configDir}/nvim";
        recursive = true;
      };
    };
  };
}
