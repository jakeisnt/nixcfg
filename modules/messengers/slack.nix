{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.messengers.slack;
in {
  options.modules.messengers.slack = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [ ]
      ++ (if config.programs.sway.enable then [ slack ] else [ slack-term ]);
  };
}
