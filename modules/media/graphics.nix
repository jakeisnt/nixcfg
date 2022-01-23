# modules/desktop/media/graphics.nix
#
# No Adobe allowed.

{ config, options, lib, pkgs, ... }:

with builtins;
with lib;
with lib.my;
let
  cfg = config.modules.media.graphics;
  # username = let name = getEnv "username";
  #            in if elem name [ "" "root" ] then username else name;
in {
  options.modules.media.graphics = {
    enable = mkBoolOpt false;
    tools.enable = mkBoolOpt true;
    raster.enable = mkBoolOpt true;
    vector.enable = mkBoolOpt true;
    sprites.enable = mkBoolOpt true;
    photo.enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      [ vlc ] ++ (if cfg.tools.enable then [
        font-manager # so many damned fonts...
        imagemagick # for image manipulation from the shell
      ] else
        [ ]) ++

      # replaces illustrator & indesign
      (if cfg.vector.enable then [ inkscape ] else [ ]) ++

      # Replaces photoshop
      (if cfg.raster.enable then [
        # krita
        gimp
        # gimpPlugins.resynthesizer2 # content-aware scaling in gimp
      ] else
        [ ]) ++

      # replaces lightroom
      (if cfg.photo.enable then [ darktable ] else [ ]) ++

      # Sprite sheets & animation
      (if cfg.sprites.enable then [ aseprite-unfree ] else [ ]);

    home.configFile = mkMerge [
      (mkIf cfg.raster.enable {
        "GIMP/2.10" = {
          source = "${configDir}/gimp";
          recursive = true;
        };
      })
      (mkIf cfg.vector.enable {
        "inkscape/templates/default.svg".source = "${configDir}/inkscape/default-template.svg";
      })
    ];

    environment.variables.PICTURES_FOLDER = "/home/${username}/pics";
  };

}
