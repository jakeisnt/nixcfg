{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.messengers.signal;
in {
  options.modules.messengers.signal = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      signal-desktop
    ];
  };
}
