{ config, lib, pkgs, ... }:

{
  xdg.userDirs = {
    enable = true;
    desktop = "$HOME/";
    download = "$HOME/";
    documents = "$HOME/";
    music = "$HOME/mus";
    pictures = "$HOME/pics";
    videos = "$HOME/pics";
  };
}
