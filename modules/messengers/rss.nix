{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.messengers.rss;
in {
  options.modules.messengers.rss = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    # feedreader: nice gui for RSS, if newsboat becomes unwieldy
    user.packages = with pkgs; [ newsboat ];
    # we want to be able to tweak .config/newsboat/urls without rebuild, so it's not included here.
    # TODO: consider tt-rss server (or mimic the protocol with another service?)
    home.configFile = {
      "newsboat/config".source = "${configDir}/newsboat/config";
    };
  };
}
