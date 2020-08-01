{ config, lib, pkgs, ... }:

{
  services.picom = {
    enable = true;
    backend = "glx";
    vSync = true;
    fade = true;
    fadeDelta = 2;
    shadow = false;
    shadowExclude = [ ];
    extraOptions = ''
      glx-no-stencil = true;
      glx-no-rebind-pixmap = true;
      use-damage = true;
      xrender-sync-fence = true;
      mark-wmwin-focused = true;
      mark-ovredir-focused = true;
    '';
  };
}
