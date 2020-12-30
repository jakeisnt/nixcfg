{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.messengers.email;
in {
  options.modules.messengers.email = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ w3m mutt neomutt offlineimap msmtp ];
    services.offlineimap = {
      enable = true;
      path = with pkgs; [ bash pass neomutt mutt ];
      onCalendar = "*:0/30"; # fetch mail every 30 minutes
    };

    home.configFile = {
      # TODO: remove secrets and finalize config files
      # "offlineimap" = { source = "${configDir}/offlineimap"; };
      # "mutt" = { source = "${configDir}/mutt"; };
      # "msmtp" = { source = "${configDir}/msmtp"; };
    };
  };
}
