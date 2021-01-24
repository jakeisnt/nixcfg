{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.xmonad;
  # colors = config.modules.theme.color;
in {
  options.modules.desktop.xmonad = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services.xserver = {
      enable = true;
      libinput.enable = true;
      displayManager.sddm.enable = true;
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = [
          haskellPackages.libmpd
          haskellPackages.xmobar
          xbrightness
          lxqt.lxqt-notificationd
          feh
          scrot
          trayer
          dzen2
          xcompmgr
          xorg.xrandr
          xscreensaver
          xsettingsd
        ] ++ (haskellPackages: [
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
          haskellPackages.xmonad
        ]);
      };
    };
  };
}

