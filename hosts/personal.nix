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

  sops.secrets.password = {};

  # TODO probably fix this, this seems bad..............
  # currently an issue in sops that prevents setting root password on the fly,
  # given nixos boot process and best way to manage secrets
  users.users.root.password = "jake";
  sops.secrets."password" = {};

  users.users.${username}.password = "jake";
  # users.users.${username}.passwordFile = config.sops.secrets."password".path;

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
