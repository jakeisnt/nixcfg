{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.logging;
    domain = config.networking.domain;
in {
  options.modules.services.logging = {
    enable = mkBoolOpt false;
    email = mkBoolOpt false;
    grafana = mkBoolOpt true;
    prometheus = mkBoolOpt true;
    loki = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    services.grafana = mkIf cfg.grafana {
      enable = true;
      domain = "grafana.${domain}";
      port = 2342;
      addr = "127.0.0.1";
    };

    services.prometheus = mkIf cfg.prometheus {
      enable = true;
      port = 9001;
    };

    services.nginx.virtualHosts.${config.services.grafana.domain} = mkIf cfg.grafana {
      forceSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:${toString config.services.grafana.port}";
        proxyWebsockets = true;
    };
  };
};
}
