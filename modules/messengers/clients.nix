{ config, lib, pkgs, ... }:

# Simple messenger client options. GUI variants require sway; CLI fallbacks used otherwise.
# Complex clients (email, rss) have their own files.

with lib;
with lib.my;

let
  hasSway = config.programs.sway.enable;
in {
  options.modules.messengers = {
    weechat.enable  = mkBoolOpt false;
    slack.enable    = mkBoolOpt false;
    deltachat.enable = mkBoolOpt false;
    signal.enable   = mkBoolOpt false;
    discord.enable  = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf config.modules.messengers.weechat.enable {
      user.packages = [ pkgs.weechat ];
    })
    (mkIf config.modules.messengers.slack.enable {
      user.packages = if hasSway then [ pkgs.slack ] else [ pkgs.slack-term ];
    })
    (mkIf config.modules.messengers.deltachat.enable {
      user.packages = if hasSway then [ pkgs.deltachat-electron ] else [];
    })
    (mkIf config.modules.messengers.signal.enable {
      user.packages = if hasSway then [ pkgs.signal-desktop ] else [ pkgs.signal-cli ];
    })
    (mkIf config.modules.messengers.discord.enable {
      # Use bleeding-edge discord to avoid the "update required" soft-lock screen.
      user.packages = if hasSway then [ pkgs.unstable.discord ] else [ pkgs.cordless ];
    })
  ];
}
