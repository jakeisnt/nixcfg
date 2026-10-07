{ options, config, lib, pkgs, ... }:

# Receive AirPlay 2 audio from Apple devices on the local network.

with lib;
with lib.my;
let cfg = config.modules.services.airplay;
in {
  options.modules.services.airplay = {
    enable = mkBoolOpt false;
    name = mkOpt types.str config.networking.hostName;
    # A system service has no PipeWire session, so play straight to ALSA.
    device = mkOpt types.str "hw:0";
  };

  config = mkIf cfg.enable {
    services.shairport-sync = {
      enable = true;
      package = pkgs.shairport-sync-airplay2;
      openFirewall = true;
      settings = {
        general = {
          name = cfg.name;
          output_backend = "alsa";
        };
        alsa.output_device = cfg.device;
      };
    };

    # AirPlay 2 timing depends on the NQPTP companion daemon.
    systemd.packages = [ pkgs.nqptp ];
    systemd.services.nqptp.wantedBy = [ "multi-user.target" ];
    systemd.services.shairport-sync = {
      wants = [ "nqptp.service" ];
      after = [ "nqptp.service" ];
    };

    networking.firewall = {
      # AirPlay 2 control, plus NQPTP's PTP event and general ports.
      allowedTCPPorts = [ 7000 ];
      allowedUDPPorts = [ 319 320 ];
      # AirPlay 2 streams use ephemeral ports.
      allowedTCPPortRanges = [ { from = 32768; to = 60999; } ];
      allowedUDPPortRanges = [ { from = 32768; to = 60999; } ];
    };
  };
}
