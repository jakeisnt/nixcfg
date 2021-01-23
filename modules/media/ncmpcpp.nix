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
      mopidycli
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
    #     User = ${secrets.username};
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
      "mopidy/mopidy.conf".text = ''
        [core]
        cache_dir = $XDG_CACHE_DIR/mopidy
        config_dir = $XDG_CONFIG_DIR/mopidy
        data_dir = $XDG_DATA_DIR/mopidy
        max_tracklist_length = 10000
        restore_state = false

        [logging]
        verbosity = 0
        format = %(levelname)-8s %(asctime)s [%(process)d:%(threadName)s] %(name)s
          %(message)s
        color = true

        [audio]
        mixer = software
        # output = pulsesink server=127.0.0.1
        output = autoaudiosink

        [proxy]
        #scheme =
        #hostname =
        # port = 6600
        #username =
        #password =

        [file]
        enabled = true
        media_dirs =
          $HOME/music
        excluded_file_extensions =
          .directory
          .html
          .jpeg
          .jpg
          .log
          .nfo
          .pdf
          .png
          .txt
          .zip
          .git
          .org
        show_dotfiles = false
        follow_symlinks = false

        [http]
        enabled = true
        hostname = 127.0.0.1
        port = 6680

        [m3u]
        enabled = true

        [softwaremixer]
        # enabled = false

        [stream]
        enabled = true
        protocols =
          http
          https
          mms
          rtmp
          rtmps
          rtsp
        #metadata_blacklist =
        #timeout = 5000

        [youtube]
        enabled = true
        autoplay_enabled = true

        [mpd]
        enabled = true

        [spotify]
        allow_network = true
        search_album_count = 20
        search_artist_count = 10
        search_track_count = 50
        enabled = true
        allow_playlists = true
        allow_cache = true

        username = ${secrets.spotify.username}
        password = ${secrets.spotify.password}
        client_id = ${secrets.spotify.clientID}
        client_secret = ${secrets.spotify.clientSecret}

        [scrobbler]
        username = ${secrets.lastFM.username}
        password = ${secrets.lastFM.password}
      '';
    };
  };
}
