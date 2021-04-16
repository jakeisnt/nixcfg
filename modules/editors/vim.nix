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
    user.packages = with pkgs; [ editorconfig-core-c neovim-nightly ];

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
