{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.messengers.deltachat;
in {
  options.modules.messengers.deltachat = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      (if config.programs.sway.enable then [ deltachat-electron ] else [ ]);
  };
}
