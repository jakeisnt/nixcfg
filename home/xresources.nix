{ config, lib, pkgs, ... }:

{
  xresources = {
    extraConfig = builtins.readFile (pkgs.fetchFromGitHub {
      owner = "arcticicestudio";
      repo = "nord-xresources";
      rev = "v0.1.0";
      sha256 = "1bhlhlk5axiqpm6l2qaij0cz4a53i9hcfsvc3hw9ayn75034xr93";
    } + "/src/nord");
  };
}
