{ config, lib, pkgs, ... }:

{
  services.picom = {
    enable = true;
    backend = "glx";
    shadow = false;
    extraOptions = ''
      glx-no-stencil = true;
      glx-no-rebind-pixmap = true;
      use-damage = true;
      vsync = true;
      xrender-sync-fence = true;
      shadow-exclude = [
        "class_g = 'Polybar'",
        "class_g = 'slop'",
        "class_g = 'firefox' && argb",
      ]
      fading = true;
      fade-delta = 2;
      mark-wmwin-focused = true;
      mark-ovredir-focused = true;
    '';
  };
}
