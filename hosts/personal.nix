{ config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
{
  time.timeZone = mkDefault "America/New_York";
  i18n.defaultLocale = mkDefault "en_US.UTF-8";

  console = {
    earlySetup = true;
    keyMap = "us";
  };

  # Only allow user creation through Nix
  users.mutableUsers = false;

  users.users.root.password = "jake";
  users.users.${username}.password = "jake";

  user.extraGroups = [
    "networkmanager"
  ];

  location = (if config.time.timeZone == "America/Los_Angeles" then {
    latitude = 43.70011;
    longitude = -79.4163;
  } else if config.time.timeZone == "Europe/Stockholm" then {
    latitude = 55.88;
    longitude = 12.5;
  } else
    { });
}
