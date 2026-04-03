{ config, options, pkgs, lib, ... }:
with builtins;
with lib;
with lib.my;
let
  cfg = config.modules.services.syncthing;
  username = let name = getEnv "username";
             in if elem name [ "" "root" ] then "jake" else name;
in {
  options.modules.services.syncthing = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.syncthing = {
      enable = true;
      user = username;
      openDefaultPorts = true;
      configDir = "/home/${username}/.config/syncthing";
      dataDir = "/home/${username}/.local/share/syncthing";

      settings = {
        folders = let
          deviceEnabled = devices: lib.elem config.networking.hostName devices;
          deviceType = devices:
            if deviceEnabled devices then "sendreceive" else "receiveonly";
        in {
          work = rec {
            devices = [ "phone" "xps" ];
            path = "/home/${username}/work";
            fsWatcherEnabled = true;
            rescanIntervalS = 3600 * 6;
            type = deviceType [ "xps" ];
            enable = deviceEnabled devices;
          };
          pics = rec {
            devices = [ "phone" "xps" ];
            path = "/home/${username}/pics";
            fsWatcherEnabled = true;
            rescanIntervalS = 3600 * 6;
            type = deviceType [ "xps" ];
            enable = deviceEnabled devices;
          };
        };

        devices = {
          xps.id =
            "M4GBTEM-N55HKKV-KLP4G5H-JFMMPUD-REYTNI6-EE4L2WS-S7KJES6-TK6Y4Q5";
          phone.id =
            "KYKZF7Z-TD62SOX-5A6LEWA-URNMNF7-CSNWD2E-ALNXGIC-P4PXDSI-4N6EHAV";
        };
      };
    };
  };
}
