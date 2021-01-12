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
    # These could consume a lot of disk space, so disable them by default.
    nix.extraOptions = mkIf cfg.preventGC ''
      keep-outputs = true
      keep-derivations = true
    '';

    environment.pathsToLink = [ "/share/nix-direnv" ];
    home.configFile = {
      "direnv/config".text =
        "source /run/current-system/sw/share/nix-direnv/direnvrc";
    };

    modules.shell.zsh.rcInit = ''eval "$(direnv hook zsh)"'';

    services.lorri.enable = true;
    systemd.user.services.lorri = {
      wantedBy = [ "ac.target" ];
      partOf = [ "ac.target" ];
      unitConfig = {
        ConditionGroup = "users";
        StopWhenUnneeded = true;
      };
    };
  };
}
