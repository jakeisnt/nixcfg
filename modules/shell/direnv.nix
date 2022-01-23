{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.direnv;
in {
  options.modules.shell.direnv = {
    enable = mkBoolOpt false;
    preventGC = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ direnv nix-direnv ];
    nix.extraOptions = mkIf cfg.preventGC ''
      keep-outputs = true
      keep-derivations = true
    '';

    environment.pathsToLink = [ "/share/nix-direnv" ];
    home.configFile = {
      "direnv/config".text =
        "source /run/current-system/sw/share/nix-direnv/direnvrc";
    };

    modules.shell.fish.rcInit = ''direnv hook fish | source'';

    home.configFile = {
      "direnv" = {
        source = "${configDir}/direnv";
        recursive = true;
      };
    };
  };
}
