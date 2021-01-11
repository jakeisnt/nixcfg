{ config, lib, ... }:

with lib; {
  time.timeZone = mkDefault "America/Los_Angeles";
  i18n.defaultLocale = mkDefault "en_US.UTF-8";

  # block facebook
  networking.extraHosts =
    "127.0.0.1 www.facebook.com facebook.com x.facebook.com";

  # for redshift mostly

  console = {
    earlySetup = true;
    # keyMap = "colemak"; TODO custom key map, as I wish
    # colors = []; can define colors here!
  };

  # Only allow user creation through Nix
  users.mutableUsers = false;

  location = (if config.time.timeZone == "America/Los_Angeles" then {
    latitude = 43.70011;
    longitude = -79.4163;
  } else if config.time.timeZone == "Europe/Stockholm" then {
    latitude = 55.88;
    longitude = 12.5;
  } else
    { });
}
