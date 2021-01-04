{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.micro;
in {
  options.modules.editors.micro = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ micro ];

    home.configFile = { "micro".source = "${configDir}/micro"; };
  };
}
