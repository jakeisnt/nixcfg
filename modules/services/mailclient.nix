# Finally, a decent open alternative to Plex!

{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.mailclient;
  domain = config.networking.domain;
in {
  options.modules.services.mailclient = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services.nginx.virtualHosts."mail.${domain}" = {
      enableACME = true;
      forceSSL = true;
      root = pkgs.roundcube;
    };
  };
}
