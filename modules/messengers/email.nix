{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.messengers.email;
in {
  options.modules.messengers.email = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ w3m mutt neomutt offlineimap msmtp notmuch ];
    services.offlineimap = {
      enable = true;
      path = with pkgs; [ bash pass neomutt ];
      onCalendar = "*:0/30"; # fetch mail every 30 minutes
    };

    environment.shellAliases = {
      mutt = "neomutt";
      mail = "neomutt";
      m = "neomutt";
    };

    home.configFile = {
      # TODO: remove secrets and finalize config files
      # "offlineimap" = { source = "${configDir}/offlineimap"; };
      # "mutt" = { source = "${configDir}/mutt"; };
      # "msmtp" = { source = "${configDir}/msmtp"; };
    };
  };
}
