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
      in if elem name [ "" "root" ] then secrets.username else name;
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
            defaultApplications = {
              "inode/directory" = "thunar.desktop";
              "text/html" = "firefox.desktop";
              "x-scheme-handler/http" = "firefox.desktop";
              "x-scheme-handler/https" = "firefox.desktop";
              "x-scheme-handler/mailto" = "userapp-Thunderbird-F73ZX0.desktop";
              "message/rfc882" = "userapp-Thunderbird-F73ZX0.desktop";
              "application/pdf" = "org.pwmt.zathura-pdf-mupdf.desktop";
              "application/zip" = "xarchiver.desktop";
              "text/" = "emacsclient.desktop";
              "image/" = "imv.desktop";
              "video/" = "mpv.desktop";
              "audio/" = "mpv.desktop";
            };
          };
          configFile = mkAliasDefinitions options.home.configFile;
          dataFile = mkAliasDefinitions options.home.dataFile;
        };
      };
    };

    users.users.${config.user.name} = mkAliasDefinitions options.user;

    nix = let users = [ "root" config.user.name ];
    in {
      trustedUsers = users;
      allowedUsers = users;
    };

    # must already begin with pre-existing PATH. Also, can't use binDir here,
    # because it contains a nix store path.
    env.PATH = [ "$XDG_CONFIG_HOME/dotfiles/bin" "$PATH" ];

    environment.extraInit = concatStringsSep "\n"
      (mapAttrsToList (n: v: ''export ${n}="${v}"'') config.env);
  };
}
