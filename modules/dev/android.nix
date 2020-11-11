{ config, options, lib, pkgs, ... }:

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
    ];
    programs.adb.enable = true;
    users.users.jake.extraGroups = ["adbusers"];
    services.udev.packages = [
      pkgs.android-udev-rules
    ];
  };
}
