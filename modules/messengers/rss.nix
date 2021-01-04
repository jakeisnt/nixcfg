{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.messengers.rss;
in {
  options.modules.messengers.rss = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    # 'liferea' provides a nice GUI for RSS if I need images and html at some point
    user.packages = with pkgs; [ newsboat ];
    # TODO: consider tt-rss server (or mimic the protocol with another service?)
    # we want to be able to tweak .config/newsboat/urls without rebuild, so it's not included here.
    home.configFile = {
      "newsboat/config".source = "${configDir}/newsboat/config";
    };
  };
}
