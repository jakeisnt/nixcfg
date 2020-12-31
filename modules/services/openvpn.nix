{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.vpn;
in {
  options.modules.services.vpn = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services.openvpn.servers = {
      # proton = { config = "config /home/jake/.config/openvpn/proton.conf"; };
    };
  };
}
