{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.search;
    domain = config.networking.domain;
in {
  options.modules.services.search = { 
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    modules.services.nginx.enable = true;

    services = {
      searx = {
        enable = true;
        configFile = "${configDir}/searx/config.yml";
      };

      nginx = {
        virtualHosts = {
          "search.${domain}" = {
            forceSSL = true;
            enableACME = true;
            locations."/" = { proxyPass = "http://127.0.0.1:8888"; };
          };
        };
      };
    };
  };
}
