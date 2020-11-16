{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.messengers.rss;
in {
  options.modules.messengers.rss = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
    ] ++ (if config.services.xserver.enable then [
      feedreader
    ] else [ newsboat ]);
    # todo: add option for server config or local config
    # todo: add list of local config options
  };
}
