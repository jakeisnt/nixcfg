# modules/desktop/media/graphics.nix
#
# No Adobe allowed.

{ config, options, lib, pkgs, inputs, ... }:

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
      (mkIf cfg.vector.enable {
        "inkscape/templates/default.svg".source = "${configDir}/inkscape/default-template.svg";
      })
    ];

    # GIMP writes this accelerator map itself.  Seed our preferred shortcuts,
    # but leave the live file mutable for custom shortcuts and GIMP upgrades.
    home-manager.users.${config.user.name}.home.activation.gimpMenu = mkIf cfg.raster.enable (
      inputs.home-manager.lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        gimp_config_dir="$XDG_CONFIG_HOME/GIMP/2.10"
        gimp_menu="$gimp_config_dir/menurc"

        $DRY_RUN_CMD mkdir -p "$gimp_config_dir"
        if [ -L "$gimp_menu" ]; then
          $DRY_RUN_CMD rm "$gimp_menu"
          $DRY_RUN_CMD cp ${escapeShellArg "${configDir}/gimp/menurc"} "$gimp_menu"
        elif [ ! -e "$gimp_menu" ]; then
          $DRY_RUN_CMD cp ${escapeShellArg "${configDir}/gimp/menurc"} "$gimp_menu"
        fi
      ''
    );

    environment.variables.PICTURES_FOLDER = "/home/${username}/pics";
  };

}
