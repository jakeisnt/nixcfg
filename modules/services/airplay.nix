{ options, config, lib, pkgs, ... }:

# AirPlay audio on the local network, in either direction:
#  - send: stream to AirPlay speakers.  PipeWire adds a sink for each
#    receiver it finds, so any app can play to one.
#  - receive: act as an AirPlay 2 speaker for Apple devices.

with lib;
with lib.my;
let cfg = config.modules.services.airplay;
in {
  options.modules.services.airplay = {
    send.enable = mkBoolOpt false;
    receive = {
      enable = mkBoolOpt false;
      name = mkOpt types.str config.networking.hostName;
      # A system service has no PipeWire session, so play straight to ALSA.
      device = mkOpt types.str "hw:0";
    };
  };

  config = mkMerge [
    (mkIf cfg.send.enable {
      assertions = [{
        assertion = config.modules.hardware.audio.enable;
        message = "modules.services.airplay.send requires modules.hardware.audio";
      }];

      # Receivers are discovered over mDNS.
      services.avahi.enable = true;

      services.pipewire = {
        # Receivers send timing and control data back over UDP.
        raopOpenFirewall = true;
        extraConfig.pipewire."10-airplay-send" = {
          "context.modules" = [{ name = "libpipewire-module-raop-discover"; }];
        };
      };
    })

    (mkIf cfg.receive.enable {
      services.shairport-sync = {
        enable = true;
        package = pkgs.shairport-sync-airplay2;
        openFirewall = true;
        settings = {
          general = {
            name = cfg.receive.name;
            output_backend = "alsa";
          };
          alsa.output_device = cfg.receive.device;
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
    })
  ];
}
