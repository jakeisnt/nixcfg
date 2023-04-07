{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  hwCfg = config.modules.hardware;
  cfg = hwCfg.bluetooth;
in {
  options.modules.hardware.bluetooth = {
    enable = mkBoolOpt false;
    audio.enable = mkBoolOpt false;
  };


  config = mkIf cfg.enable (mkMerge [
    {
      hardware.bluetooth = {
        enable = true;
        package = pkgs.bluezFull;
      };
      services.blueman.enable = true;
      user.extraGroups = [ "bluetooth" "lp"];
    }
    (mkIf cfg.audio.enable {
      systemd.user.services.mpris-proxy = {
        Unit.Description = "Mpris proxy";
        Unit.After = [ "network.target" "sound.target" ];
        Service.ExecStart = "${pkgs.bluez}/bin/mpris-proxy";
        Install.WantedBy = [ "default.target" ];
      };

      hardware.bluetooth.settings = {
        General = {
          Enable = "Source,Sink,Media,Socket";
          # ControllerMode = "bredr"; # disable Bluetooth LE
        };
      };
    })
  ]);
}
