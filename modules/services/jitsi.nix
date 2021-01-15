# Finally, a decent open alternative to Plex!

{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.services.jitsi;
  domain = config.networking.domain;
in {
  options.modules.services.jitsi = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services = {
      jitsi-meet = {
        enable = true;
        hostName = "jitsi.${domain}";
        config = {
          enableWelcomePage = false;
          prejoinPageEnabled = true;
        };
        interfaceConfig = {
          SHOW_JITSI_WATERMARK = false;
          SHOW_WATERMARK_FOR_GUESTS = false;
        };
      };

      jitsi-videobridge.openFirewall = true;
    };

    networking.firewall = {
      allowedTCPPorts = [ 5349 5350 ];
      allowedUDPPorts = [ 80 443 3478 3479 ];
    };

    user.extraGroups = [ "jellyfin" ];
  };
}
