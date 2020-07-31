{ pkgs, lib, ... }:
let inherit (import ./helpers.nix) justLinux justLinuxAttrs;
in {
  imports = [ ./nvim ./git.nix ./tmux.nix ./shells.nix ];

  home.packages = with pkgs;
    [ ranger ripgrep valgrind wget coreutils ] ++ justLinux [ fzf syncthing ];

  programs.home-manager.enable = true;
  home.sessionVariables.EDITOR = "nvim";
}
