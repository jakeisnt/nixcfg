{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.scanner;
in {
  options.modules.hardware.scanner = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    hardware.sane = {
      enable = true;
      # needed to support epson
      extraBackends = with pkgs; [ epkowa ];
    };
    user.extraGroups = [ "scanner" "lp" ];
  };
}
