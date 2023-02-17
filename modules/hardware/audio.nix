{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.audio;
in {
  options.modules.hardware.audio = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    sound.enable = true;
    xdg.portal = mkIf config.modules.desktop.sway.enable {
      enable = true;
      wlr.enable = true;
      extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
    };

    hardware.pulseaudio.enable = mkForce false;

    # https://github.com/NixOS/nixpkgs/issues/102547
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
      jack.enable = true;
    };

    user.packages = with pkgs; [
      # communicate with pipewire via `pactl`
      pulseaudio
      # dynamically linked to; supports camera connection
      libcamera
    ];
  };
}
