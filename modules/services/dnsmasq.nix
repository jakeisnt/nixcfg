{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.dnsmasq;
in {
  options.modules.services.dnsmasq = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # use dnsmasq to cache dns
    services.dnsmasq = {
      enable = true;
      servers = [ "8.8.8.8" "8.8.4.4" ];

      extraConfig = ''
        interface=lo
        bind-interfaces
        listen-address=127.0.0.1
        cache-size=1000
        no-negcache
      '';
    };
  };
}
