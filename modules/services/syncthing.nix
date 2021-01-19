{ config, options, pkgs, lib, ... }:
with builtins;
with lib;
with lib.my;
let
  username = let name = getEnv "username";
  in if elem name [ "" "root" ] then "jake" else name;
in {
  options.modules.services.syncthing = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf config.modules.services.syncthing.enable {
    services.syncthing = {
      enable = true;
      user = username;
      # openDefaultPorts = true;
      configDir = "/home/${username}/.config/syncthing";
      dataDir = "/home/${username}/.local/share/syncthing";

      declarative = {
        devices = {
          xps.id =
            "M4GBTEM-N55HKKV-KLP4G5H-JFMMPUD-REYTNI6-EE4L2WS-S7KJES6-TK6Y4Q5";
          phone.id =
            "SGMQCCV-JQ6FIFB-PSKOJUE-DYV34FO-N26QZVV-XNSIV3X-TWBOHYF-KPWPJQF";
        };
        folders = let
          deviceEnabled = devices: lib.elem config.networking.hostName devices;
          deviceType = devices:
            if deviceEnabled devices then "sendreceive" else "receiveonly";
        in {
          test = rec {
            devices = [ "xps" "phone" ];
            path = "/home/${secrets.username}/test";
            watch = true;
            rescanInterval = 3600 * 6;
            type = deviceType [ "xps" ];
            enable = deviceEnabled devices;
          };
          # work = rec {
          #   devices = [ # "pi" "vultr"
          #     "phone"
          #     "xps"
          #   ];
          #   path = "/home/${secrets.username}/work";
          #   watch = true;
          #   rescanInterval = 3600 * 6;
          #   enable = deviceEnabled devices;
          # };
          # secrets = rec {
          #   devices = [ "kuro" "shiro" "ao" "aka" ];
          #   path = "/home/${config.my.username}/.secrets";
          #   watch = true;
          #   rescanInterval = 3600;
          #   type = deviceType [ "kuro" "shiro" ];
          #   enable = deviceEnabled devices;
          # };
        };
      };
    };
  };
}
