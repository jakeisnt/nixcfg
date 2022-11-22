{ config, options, lib, home-manager, ... }:

with lib;
with lib.my;
{
  options = with types; {
    user = mkOpt attrs { };

    home = {
      file = mkOpt' attrs { } "Files to place directly in $HOME";
      configFile = mkOpt' attrs { } "Files to place in $XDG_CONFIG_HOME";
      dataFile = mkOpt' attrs { } "Files to place in $XDG_DATA_HOME";
    };

    env = mkOption {
      type = attrsOf (oneOf [ str path (listOf (either str path)) ]);
      apply = mapAttrs (n: v:
        if isList v then
          concatMapStringsSep ":" (x: toString x) v
        else
          (toString v));
      default = { };
      description = "TODO";
    };
  };

  config = {
    user = {
      description = "The primary user account";
      extraGroups = [ "wheel" ];
      isNormalUser = true;
      name = let name = builtins.getEnv "USER";
             in if elem name [ "jake" "root" ] then username else name;
      uid = 1000;
    };

    # Install user packages to /etc/profiles instead. Necessary for
    # nixos-rebuild build-vm to work.
    home-manager = {
      useUserPackages = true;

      # I only need a subset of home-manager's capabilities. That is, access to
      # its home.file, home.xdg.configFile and home.xdg.dataFile so I can deploy
      # files easily to my $HOME, but 'home-manager.users.hlissner.home.file.*'
      # is much too long and harder to maintain, so I've made aliases in:
      #
      #   home.file        ->  home-manager.users.jake.home.file
      #   home.configFile  ->  home-manager.users.jake.home.xdg.configFile
      #   home.dataFile    ->  home-manager.users.jake.home.xdg.dataFile
      users.${config.user.name} = {
        home = {
          file = mkAliasDefinitions options.home.file;
          # Necessary for home-manager to work with flakes, otherwise it will
          # look for a nixpkgs channel.
          stateVersion = config.system.stateVersion;
        };
        xdg = {
          enable = true;
          mime.enable = true;
          mimeApps = {
            enable = true;
            # https://github.com/Mic92/dotfiles/blob/master/nixpkgs-config/modules/default-apps.nix
            defaultApplications =
              let
                # take from the respective mimetype files
                images = [
                  "image/bmp"
                  "image/gif"
                  "image/jpeg"
                  "image/jpg"
                  "image/pjpeg"
                  "image/png"
                  "image/tiff"
                  "image/x-bmp"
                  "image/x-gray"
                  "image/x-icb"
                  "image/x-ico"
                  "image/x-png"
                  "image/x-portable-anymap"
                  "image/x-portable-bitmap"
                  "image/x-portable-graymap"
                  "image/x-portable-pixmap"
                  "image/x-xbitmap"
                  "image/x-xpixmap"
                  "image/x-pcx"
                  "image/svg+xml"
                  "image/svg+xml-compressed"
                  "image/vnd.wap.wbmp;image/x-icns"
                ];
                urls = [
                  "text/html"
                  "x-scheme-handler/http"
                  "x-scheme-handler/https"
                  "x-scheme-handler/about"
                  "x-scheme-handler/unknown"
                ];
                documents = [
                  "application/vnd.comicbook-rar"
                  "application/vnd.comicbook+zip"
                  "application/x-cb7"
                  "application/x-cbr"
                  "application/x-cbt"
                  "application/x-cbz"
                  "application/x-ext-cb7"
                  "application/x-ext-cbr"
                  "application/x-ext-cbt"
                  "application/x-ext-cbz"
                  "application/x-ext-djv"
                  "application/x-ext-djvu"
                  "image/vnd.djvu+multipage"
                  "application/x-bzdvi"
                  "application/x-dvi"
                  "application/x-ext-dvi"
                  "application/x-gzdvi"
                  "application/x-bzpdf"
                  "application/x-ext-pdf"
                  "application/x-gzpdf"
                  "application/x-xzpdf"
                  "application/postscript"
                  "application/x-bzpostscript"
                  "application/x-gzpostscript"
                  "application/x-ext-eps"
                  "application/x-ext-ps"
                  "image/x-bzeps"
                  "image/x-eps"
                  "image/x-gzeps"
                  "image/tiff"
                  "application/oxps"
                  "application/vnd.ms-xpsdocument"
                  "application/illustrator"
                ];
                audioVideo = [
                  "application/ogg"
                  "application/x-ogg"
                  "application/mxf"
                  "application/sdp"
                  "application/smil"
                  "application/x-smil"
                  "application/streamingmedia"
                  "application/x-streamingmedia"
                  "application/vnd.rn-realmedia"
                  "application/vnd.rn-realmedia-vbr"
                  "audio/aac"
                  "audio/x-aac"
                  "audio/vnd.dolby.heaac.1"
                  "audio/vnd.dolby.heaac.2"
                  "audio/aiff"
                  "audio/x-aiff"
                  "audio/m4a"
                  "audio/x-m4a"
                  "application/x-extension-m4a"
                  "audio/mp1"
                  "audio/x-mp1"
                  "audio/mp2"
                  "audio/x-mp2"
                  "audio/mp3"
                  "audio/x-mp3"
                  "audio/mpeg"
                  "audio/mpeg2"
                  "audio/mpeg3"
                  "audio/mpegurl"
                  "audio/x-mpegurl"
                  "audio/mpg"
                  "audio/x-mpg"
                  "audio/rn-mpeg"
                  "audio/musepack"
                  "audio/x-musepack"
                  "audio/ogg"
                  "audio/scpls"
                  "audio/x-scpls"
                  "audio/vnd.rn-realaudio"
                  "audio/wav"
                  "audio/x-pn-wav"
                  "audio/x-pn-windows-pcm"
                  "audio/x-realaudio"
                  "audio/x-pn-realaudio"
                  "audio/x-ms-wma"
                  "audio/x-pls"
                  "audio/x-wav"
                  "video/mpeg"
                  "video/x-mpeg2"
                  "video/x-mpeg3"
                  "video/mp4v-es"
                  "video/x-m4v"
                  "video/mp4"
                  "application/x-extension-mp4"
                  "video/divx"
                  "video/vnd.divx"
                  "video/msvideo"
                  "video/x-msvideo"
                  "video/ogg"
                  "video/quicktime"
                  "video/vnd.rn-realvideo"
                  "video/x-ms-afs"
                  "video/x-ms-asf"
                  "audio/x-ms-asf"
                  "application/vnd.ms-asf"
                  "video/x-ms-wmv"
                  "video/x-ms-wmx"
                  "video/x-ms-wvxvideo"
                  "video/x-avi"
                  "video/avi"
                  "video/x-flic"
                  "video/fli"
                  "video/x-flc"
                  "video/flv"
                  "video/x-flv"
                  "video/x-theora"
                  "video/x-theora+ogg"
                  "video/x-matroska"
                  "video/mkv"
                  "audio/x-matroska"
                  "application/x-matroska"
                  "video/webm"
                  "audio/webm"
                  "audio/vorbis"
                  "audio/x-vorbis"
                  "audio/x-vorbis+ogg"
                  "video/x-ogm"
                  "video/x-ogm+ogg"
                  "application/x-ogm"
                  "application/x-ogm-audio"
                  "application/x-ogm-video"
                  "application/x-shorten"
                  "audio/x-shorten"
                  "audio/x-ape"
                  "audio/x-wavpack"
                  "audio/x-tta"
                  "audio/AMR"
                  "audio/ac3"
                  "audio/eac3"
                  "audio/amr-wb"
                  "video/mp2t"
                  "audio/flac"
                  "audio/mp4"
                  "application/x-mpegurl"
                  "video/vnd.mpegurl"
                  "application/vnd.apple.mpegurl"
                  "audio/x-pn-au"
                  "video/3gp"
                  "video/3gpp"
                  "video/3gpp2"
                  "audio/3gpp"
                  "audio/3gpp2"
                  "video/dv"
                  "audio/dv"
                  "audio/opus"
                  "audio/vnd.dts"
                  "audio/vnd.dts.hd"
                  "audio/x-adpcm"
                  "application/x-cue"
                  "audio/m3u"
                ];

                archives = [
                  "application/bzip2"
                  "application/gzip"
                  "application/vnd.android.package-archive"
                  "application/vnd.ms-cab-compressed"
                  "application/vnd.debian.binary-package"
                  "application/x-7z-compressed"
                  "application/x-7z-compressed-tar"
                  "application/x-ace"
                  "application/x-alz"
                  "application/x-ar"
                  "application/x-archive"
                  "application/x-arj"
                  "application/x-brotli"
                  "application/x-bzip-brotli-tar"
                  "application/x-bzip"
                  "application/x-bzip-compressed-tar"
                  "application/x-bzip1"
                  "application/x-bzip1-compressed-tar"
                  "application/x-cabinet"
                  "application/x-cd-image"
                  "application/x-compress"
                  "application/x-compressed-tar"
                  "application/x-cpio"
                  "application/x-chrome-extension"
                  "application/x-deb"
                  "application/x-ear"
                  "application/x-ms-dos-executable"
                  "application/x-gtar"
                  "application/x-gzip"
                  "application/x-gzpostscript"
                  "application/x-java-archive"
                  "application/x-lha"
                  "application/x-lhz"
                  "application/x-lrzip"
                  "application/x-lrzip-compressed-tar"
                  "application/x-lz4"
                  "application/x-lzip"
                  "application/x-lzip-compressed-tar"
                  "application/x-lzma"
                  "application/x-lzma-compressed-tar"
                  "application/x-lzop"
                  "application/x-lz4-compressed-tar"
                  "application/x-ms-wim"
                  "application/x-rar"
                  "application/x-rar-compressed"
                  "application/x-rpm"
                  "application/x-source-rpm"
                  "application/x-rzip"
                  "application/x-rzip-compressed-tar"
                  "application/x-tar"
                  "application/x-tarz"
                  "application/x-tzo"
                  "application/x-stuffit"
                  "application/x-war"
                  "application/x-xar"
                  "application/x-xz"
                  "application/x-xz-compressed-tar"
                  "application/x-zip"
                  "application/x-zip-compressed"
                  "application/x-zstd-compressed-tar"
                  "application/x-zoo"
                  "application/zip"
                  "application/zstd"
                ];
                directory = [
                  "inode/directory"
                ];
                code = [
                  "text/english"
                  "text/plain"
                  "text/x-makefile"
                  "text/x-c++hdr"
                  "text/x-c++src"
                  "text/x-chdr"
                  "text/x-csrc"
                  "text/x-java"
                  "text/x-moc"
                  "text/x-pascal"
                  "text/x-tcl"
                  "text/x-tex"
                  "application/x-shellscript"
                  "text/x-c"
                  "text/x-c++"
                ];
              in
                {
                  "x-scheme-handler/mailto" = [ "userapp-Thunderbird-F73ZX0.desktop" ];
                  "application/pdf" = [ "org.pwmt.zathura-pdf-mupdf.desktop" ];
                } // (lib.genAttrs code (_: [ "emacsclient.desktop" ]))
                // (lib.genAttrs images (_: [ "imv.desktop" ]))
                // (lib.genAttrs urls (_: [ "firefox.desktop" ]))
                // (lib.genAttrs audioVideo (_: [ "mpv.desktop" ]))
                # TODO the following are currently unused - install as needed
                // (lib.genAttrs documents (_: [ "org.gnome.Evince.desktop" ]))
                // (lib.genAttrs archives (_: [ "org.gnome.FileRoller.desktop" ]))
                // (lib.genAttrs archives (_: [ "org.gnome.Nautilus.desktop" ]));
          };
          configFile = mkAliasDefinitions options.home.configFile;
          dataFile = mkAliasDefinitions options.home.dataFile;
        };
      };
    };

    users.users.${config.user.name} = mkAliasDefinitions options.user;

    nix = let users = [ "root" config.user.name ];
          in {
            settings.trusted-users = users;
            settings.allowed-users = users;
          };

    # must already begin with pre-existing PATH. Also, can't use binDir here,
    # because it contains a nix store path.
    env.PATH = [ "$XDG_CONFIG_HOME/dotfiles/bin" "$PATH" ];

    environment.extraInit = concatStringsSep "\n"
      (mapAttrsToList (n: v: ''export ${n}="${v}"'') config.env);
  };
}
