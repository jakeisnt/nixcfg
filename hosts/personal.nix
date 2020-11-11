{ config, lib, ... }:

with lib;
{
  # networking.hosts =
  #   let hostConfig = {
  #         "192.168.1.2"  = [ "ao" ];
  #         "192.168.1.3"  = [ "aka" ];
  #         "192.168.1.10" = [ "kuro" ];
  #         "192.168.1.11" = [ "shiro" ];
  #         "192.168.1.12" = [ "midori" ];
  #       };
  #       hosts = flatten (attrValues hostConfig);
        # hostName = config.networking.hostName;
  #   in mkIf (builtins.elem hostName hosts) hostConfig;

  time.timeZone = mkDefault "Europe/Stockholm";
  i18n.defaultLocale = mkDefault "en_US.UTF-8";

  # For redshift, mainly
  location = (if config.time.timeZone == "America/Los_Angeles" then {
    latitude = 43.70011;
    longitude = -79.4163;
  } else if config.time.timeZone == "Europe/Stockholm" then {
    latitude = 55.88;
    longitude = 12.5;
  } else {});
}
