{ config, lib, pkgs, ... }:

with lib;
with lib.my;
{
  config = {
    env.TERMINAL = "ghostty";
    user.packages = with pkgs; [ ghostty ];
    home.configFile = {
      "ghostty/config".source = mkOutOfStoreSymlink "${configDir}/ghostty/config";
    };
  };
}
