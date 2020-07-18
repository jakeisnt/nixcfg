{ pkgs, ... }:
let
  plugins = with pkgs.vimPlugins; [
    vim-gitgutter
    vim-fugitive
    vim-rhubarb
    vim-vinegar
    vim-dispatch
    vim-easy-align
    vim-move
    tcomment_vim

    vim-nix
    vim-easymotion
    vim-commentary
    fzf-vim
    vim-surround
    vim-unimpaired
    tabular
    vim-repeat
    vim-indent-object

    nerdtree

    vim-airline
    vim-airline-themes

    ale
  ];

in {
  imports = [
    ./main.nix
  ];
  programs.neovim.plugins = plugins;
}
