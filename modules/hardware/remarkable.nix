{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.remarkable;
in {
  options.modules.hardware.remarkable = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable { user.packages = with pkgs; [ rmapi ]; };
}
