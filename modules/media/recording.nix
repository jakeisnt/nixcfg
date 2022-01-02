# modules/desktop/media/recording.nix
#
# OBS to capture footage/stream, audacity for audio, handbrake to encode it all.
# This, paired with DaVinci Resolve for video editing (on my Windows system) and
# I have what I need for youtube videos and streaming.

{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.media.recording;
in
{
  options.modules.media.recording = {
    enable = mkBoolOpt false;
    audio.enable = mkBoolOpt true;
    video.enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      # for recording and remastering audio
      (
        if cfg.audio.enable then [
          # audacity
        ] else []
      ) ++ # for longer term streaming/recording the screen
      (if cfg.video.enable then [(wrapOBS {
      plugins = with obs-studio-plugins; [
        wlrobs
      ];
    }) handbrake ] else []);
  };
}
