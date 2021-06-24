{ config, options, lib, pkgs, inputs, ... }:

# If I ever want to improve my Vim configuration,
# https://github.com/gvolpe/nix-config/tree/master/home/programs/neovim
# is a great source.

with lib;
with lib.my;
let cfg = config.modules.editors.vim;
in {
  options.modules.editors.vim = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    nixpkgs.overlays = with inputs; [ neovim-nightly-overlay.overlay ];
    user.packages = with pkgs; [ editorconfig-core-c ];

    programs.neovim = {
      package = pkgs.neovim-nightly;
      enable = true;
      defaultEditor = false; # this is configured by me elsewhere
      # viAlias = true;
      # vimAlias = true;

      configure = {
        customRC = ''
          lua require('init')
        '';

        packages.myPlugins = with pkgs.vimPlugins; {

          start = [
            vim-polyglot
            vim-commentary
            nvim-treesitter
            nvim-lspconfig
            completion-nvim
            telescope-nvim
            popup-nvim
            plenary-nvim
            onedark-vim
            lualine-nvim
            which-key-nvim
            neogit
            vim-gitgutter
            auto-pairs
          ];
        };
      };

      # plugins = with pkgs.vimPlugins; [
      #   vim-polyglot
      #   vim-commentary
      #   nvim-treesitter
      #   nvim-lspconfig
      #   completion-nvim
      #   telescope-nvim
      #   popup-nvim
      #   plenary-nvim
      #   onedark-vim
      #   lualine-nvim
      #   which-key-nvim
      #   neogit
      #   vim-gitgutter
      #   auto-pairs
      # ];
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
