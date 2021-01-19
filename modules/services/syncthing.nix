{ config, options, pkgs, lib, ... }:
with builtins;
with lib;
with lib.my;
let
  cfg = config.modules.services.syncthing;
  username = let name = getEnv "username";
  in if elem name [ "" "root" ] then "jake" else name;

  domain = config.networking.domain;
in {
  options.modules.services.syncthing = {
    enable = mkBoolOpt false;
    server = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    # modules.services.acme.enable = true;
    # modules.services.nginx.enable = true;
    #
    # services.nginx = mkIf cfg.server {
    #   "syncthing.${domain}" = {
    #     forceSSL = true;
    #     enableACME = true;
    #     locations."/" = {
    #       proxyPass = "http://localhost:21027";
    #       proxyWebsockets = true;
    #   };
    # };

    services.syncthing = {
      enable = true;
      user = username;
      openDefaultPorts = true;
      configDir = "/home/${username}/.config/syncthing";
      dataDir = "/home/${username}/.local/share/syncthing";

      declarative = {
        devices = {
          xps.id =
            "M4GBTEM-N55HKKV-KLP4G5H-JFMMPUD-REYTNI6-EE4L2WS-S7KJES6-TK6Y4Q5";
          phone.id =
            "SGMQCCV-JQ6FIFB-PSKOJUE-DYV34FO-N26QZVV-XNSIV3X-TWBOHYF-KPWPJQF";
          vultr.id =
            "RGIZAD-VHMLCCH-PFONWDZ-JEHKFAS-QG2ODRM-L7CWZBZ-SYD3LJF-DXZC3QI";
        };
        folders = let
          deviceEnabled = devices: lib.elem config.networking.hostName devices;
          deviceType = devices:
            if deviceEnabled devices then "sendreceive" else "receiveonly";
        in {
          work = rec {
            devices = [ "phone" "xps" "vultr" ];
            path = "/home/${secrets.username}/work";
            watch = true;
            rescanInterval = 3600 * 6;
            type = deviceType [ "xps" ];
            enable = deviceEnabled devices;
          };
          pics = rec {
            devices = [ "phone" "xps" "vultr" ];
            path = "/home/${secrets.username}/pics";
            watch = true;
            rescanInterval = 3600 * 6;
            type = deviceType [ "xps" ];
            enable = deviceEnabled devices;
          };
        };
      };
    };
  };
}
