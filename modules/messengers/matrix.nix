{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.messengers.matrix;
in {
  options.modules.messengers.matrix = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      element-desktop
    ];
  };
}
