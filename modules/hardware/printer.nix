{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.printer;
in {
  options.modules.hardware.printer = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services.printing = {
      enable = true;
      drivers = [ ];
    };

    modules.services.avahi.enable = true;

    user.extraGroups = [ "scanner" "lp" ];
  };
}
