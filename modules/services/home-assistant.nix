{ options, config, lib, pkgs, ... }:

# Home Assistant as the home's automation brain, with Matter (over Thread
# or Wi-Fi) for sensors.  Automations can send OSC to SuperCollider, so
# sensors can steer the music.

with lib;
with lib.my;
let
  cfg = config.modules.services.home-assistant;
  oscsend = "${pkgs.liblo}/bin/oscsend";
in {
  options.modules.services.home-assistant = {
    enable = mkBoolOpt false;
    # Serve the web UI only on these interfaces.
    interfaces = mkOpt (types.listOf types.str) [ ];
    thread = {
      # A Thread radio running OpenThread RCP firmware, such as
      # /dev/serial/by-id/usb-...  Leave null to use an existing border
      # router (IKEA DIRIGERA, HomePod, Apple TV, ...).
      radio = mkOpt (types.nullOr types.str) null;
      # The LAN interfaces that should route to the Thread network.
      backboneInterfaces = mkOpt (types.listOf types.str) [ ];
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      services.matter-server.enable = true;

      services.home-assistant = {
        enable = true;
        extraComponents = [
          # Needed to finish onboarding.
          "analytics"
          "google_translate"
          "met"
          "radio_browser"
          "shopping_list"
          # Sensors.
          "matter"
          "thread"
          "otbr"
        ];
        config = {
          default_config = { };
          http.server_port = 8123;
          # Keep automations editable from the UI.
          "automation ui" = "!include automations.yaml";
          "scene ui" = "!include scenes.yaml";
          "script ui" = "!include scripts.yaml";
          # Send an OSC float to sclang, e.g. from an automation:
          #   action: shell_command.supercollider
          #   data: { path: /lux, value: "{{ states('sensor.lux') }}" }
          shell_command.supercollider =
            "${oscsend} 127.0.0.1 57120 {{ path }} f {{ value }}";
        };
      };

      systemd.tmpfiles.rules = map
        (f: "f ${config.services.home-assistant.configDir}/${f} 0644 hass hass")
        [ "automations.yaml" "scenes.yaml" "scripts.yaml" ];

      # Accept the routes Thread border routers (e.g. a HomePod) announce
      # for their mesh, so Matter can reach Thread devices.  NetworkManager
      # handles these itself on the interfaces it manages.
      boot.kernel.sysctl."net.ipv6.conf.*.accept_ra_rt_info_max_plen" = 64;

      networking.firewall.interfaces = genAttrs cfg.interfaces
        (_: { allowedTCPPorts = [ 8123 ]; });
    }

    (mkIf (cfg.thread.radio != null) {
      services.openthread-border-router = {
        enable = true;
        radio.device = cfg.thread.radio;
        backboneInterfaces = cfg.thread.backboneInterfaces;
      };
    })
  ]);
}
