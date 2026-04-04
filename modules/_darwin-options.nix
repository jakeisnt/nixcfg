# modules/_darwin-options.nix
#
# Darwin analogue of modules/options.nix. Defines the same top-level options
# (user.packages, home.*, env) but wires them into nix-darwin + home-manager
# instead of NixOS. The _ prefix excludes this from Linux auto-import.
#
# Key difference from the NixOS version:
#   - user.packages aliases to home-manager.users.<name>.home.packages
#     (nix-darwin has no users.users.<name>.packages)
#   - user is structured (not attrs) so list-merging works across modules
#   - No XDG MIME associations (macOS handles file types differently)

{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
{
  options = with types; {
    user = {
      packages = mkOpt (listOf package) [];
      shell    = mkOpt (nullOr package) null;
    };

    home = {
      file       = mkOpt' attrs {} "Files to place directly in $HOME";
      configFile = mkOpt' attrs {} "Files to place in $XDG_CONFIG_HOME";
      dataFile   = mkOpt' attrs {} "Files to place in $XDG_DATA_HOME";
    };

    env = mkOption {
      type  = attrsOf (oneOf [ str path (listOf (either str path)) ]);
      apply = mapAttrs (n: v:
        if isList v
        then concatMapStringsSep ":" (x: toString x) v
        else toString v);
      default     = {};
      description = "Environment variables injected via environment.extraInit";
    };
  };

  config = {
    home-manager = {
      useUserPackages = true;
      users.${username} = {
        home = {
          enableNixpkgsReleaseCheck = false;
          file          = mkAliasDefinitions options.home.file;
          stateVersion  = "24.11";
          homeDirectory = builtins.toPath darwinHomeDir;
          packages      = mkAliasDefinitions options.user.packages;
        };
        manual = {
          html.enable = false;
          json.enable = false;
          manpages.enable = false;
        };
        xdg = {
          enable     = true;
          configFile = mkAliasDefinitions options.home.configFile;
          dataFile   = mkAliasDefinitions options.home.dataFile;
        };
      };
    };

    users.users.${username} =
      {
        home = builtins.toPath darwinHomeDir;
      }
      // optionalAttrs (config.user.shell != null) {
        shell = config.user.shell;
      };

    nix.settings = {
      trusted-users = [ "root" username ];
      allowed-users = [ "root" username ];
    };

    env.PATH = [ "$XDG_CONFIG_HOME/dotfiles/bin" "$PATH" ];

    environment.extraInit = concatStringsSep "\n"
      (mapAttrsToList (n: v: ''export ${n}="${v}"'') config.env);
  };
}
