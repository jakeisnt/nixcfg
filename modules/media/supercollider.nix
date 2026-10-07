# Generative and ambient music with SuperCollider.  It plays through
# PipeWire's JACK emulation, so it can reach any PipeWire sink, AirPlay
# speakers included.

{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.media.supercollider;
in
{
  options.modules.media.supercollider = {
    enable = mkBoolOpt false;
    # Run the user's PipeWire from boot, so music plays without a login.
    headless = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = config.modules.hardware.audio.enable;
      message = "modules.media.supercollider requires modules.hardware.audio";
    }];

    user.packages = with pkgs; [
      supercollider-with-sc3-plugins
      # sclang links Qt, which needs a display unless told otherwise.
      (writeShellScriptBin "sclang-headless" ''
        QT_QPA_PLATFORM=offscreen exec ${supercollider-with-sc3-plugins}/bin/sclang "$@"
      '')
    ];

    user.linger = mkIf cfg.headless true;
  };
}
