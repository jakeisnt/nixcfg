{ config, options, lib, pkgs, ... }:
# https://nixos.wiki/wiki/Android
with lib;
with lib.my;
let cfg = config.modules.dev.android;
in {
  options.modules.dev.android = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      android-studio
      androidenv.androidPkgs_9_0.androidsdk
      glibc
    ];
    programs.adb.enable = true;
    users.users.jake.extraGroups = ["adbusers"];
    services.udev.packages = [
      pkgs.android-udev-rules
    ];
  };
}
