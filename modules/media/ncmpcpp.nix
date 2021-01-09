{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.media.ncmpcpp;
  mopidyEnv = pkgs.buildEnv {
    name = "mopidy-with-extensions";
    paths = closePropagation (with pkgs; [
      mopidy-spotify
      mopidy-youtube
      mopidy-mpd
      mopidy-scrobbler
    ]);
    pathsToLink = [ "/${pkgs.mopidyPackages.python.sitePackages}" ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      makeWrapper ${pkgs.mopidy}/bin/mopidy $out/bin/mopidy \
        --prefix PYTHONPATH : $out/${pkgs.mopidyPackages.python.sitePackages}
    '';
  };
in {
  options.modules.media.ncmpcpp = {
    enable = mkBoolOpt false;
    # mopidy.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      ncmpcpp
      (writeScriptBin "mpd" ''
        #!${stdenv.shell}
        exec ${mopidyEnv}/bin/mopidy &> /dev/null & disown
      '')
    ];

    environment.shellAliases = {
      music = "mpd & ncmpcpp";
      mus = "mpd & ncmpcpp";
    };

    # as a systemd service, mopidy can't properly connect to pulseaudio
    # so this is on hold until that's figured out.
    # systemd.services.mopidy = {
    #   wantedBy = [ "multi-user.target" ];
    #   after = [ "network.target" "sound.target" "network-online.target" ];
    #   description = "mopidy music player daemon";
    #   serviceConfig = {
    #     ExecStart = "${mopidyEnv}/bin/mopidy";
    #     User = "jake";
    #   };
    # };

    # the official mopidy service has the same problem,
    # and another: when mopidy is running as a privileged user,
    # it by default pulls its configuration from /etc (with no way
    # to reconfigure for the official nixos package), which
    # is inconsistent with what we want for our configuration.

    env.NCMPCPP_HOME = "$XDG_CONFIG_HOME/ncmpcpp";

    # Symlink these one at a time because ncmpcpp writes other files to
    # ~/.config/ncmpcpp, so it needs to be writeable.
    home.configFile = {
      "ncmpcpp/config".source = "${configDir}/ncmpcpp/config";
      "ncmpcpp/bindings".source = "${configDir}/ncmpcpp/bindings";
      "mopidy/mopidy.conf".source = "${secretsDir}/mopidy/mopidy.conf";
    };
  };
}
