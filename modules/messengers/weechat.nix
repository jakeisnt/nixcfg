{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.messengers.weechat;
in {
  options.modules.messengers.weechat = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ weechat ];

    env = { WEECHAT_HOME = "${configDir}/weechat"; };
  };
}
