# I don't actively use VSCode, but it's good to have a configuration handy
# because it's become the industry standard for text editors.
#
# It was a pain to get these hashes working.
# I retrieved the real URL from https://github.com/NixOS/nixpkgs/blob/master/pkgs/misc/vscode-extensions/mktplcExtRefToFetchArgs.nix
# and wrote a quick bash script to fetch the hash provided the url - go check it out in the bin/ folder

{ config, options, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors.vscode;
  extensions = with pkgs.vscode-extensions;
    (
      [ bbenoist.nix vscodevim.vim eugleo.magic-racket ]
      ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
        {
          name = "vscode-wakatime";
          publisher = "WakaTime";
          version = "5.0.1";
          sha256 = "0wahh7kmrdzcrcp2lj71qxsxbmgdq97k60aw2jxxqlmdfy407nnh";
        }
        {
          name = "stilla";
          publisher = "jakeisnt";
          version = "0.0.1";
          sha256 = "1hwmv2545qmdx7s6mf3ba0qf6217xssws1w2018s6s75sbcxmga4";
        }
        {
          name = "vscode-direnv";
          publisher = "Rubymaniac";
          version = "0.0.2";
          sha256 = "1gml41bc77qlydnvk1rkaiv95rwprzqgj895kxllqy4ps8ly6nsd";
        }
        {
          name = "leadermode";
          publisher = "michaelgriscom";
          version = "0.2.0";
          sha256 = "184b8g0rgfasippvfvr6bslaxfm34bqa2mgzr0y0dphj26fyw2wn";
        }
      ] ++ (
        if config.modules.dev.node.enable then
          pkgs.vscode-utils.extensionsFromVscodeMarketplace [
            {
              name = "vscode-eslint";
              publisher = "dbaeumer";
              version = "2.1.14";
              sha256 = "113w2iis4zi4z3sqc3vd2apyrh52hbh2gvmxjr5yvjpmrsksclbd";
            }
          ]
        else
          []
      ) ++ (
        if config.modules.dev.cc.enable then [
          xaver.clang-format
          ms-vscode.cpptools
        ] else
          []
      ) ++ (
        if config.modules.dev.rust.enable then
          [ matklad.rust-analyzer ]
        else
          []
      )
    ) ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "remote-ssh-edit";
        publisher = "ms-vscode-remote";
        version = "0.47.2";
        sha256 = "1hp6gjh4xp2m1xlm1jsdzxw9d8frkiidhph6nvl24d0h8z34w49g";
      }
    ];
  vscodium-with-extensions = pkgs.vscode-with-extensions.override {
    vscode = pkgs.vscodium;
    vscodeExtensions = extensions;
  };
in
{
  options.modules.editors.vscode = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ vscodium-with-extensions ];
    # VSCodium writes these files when settings are changed in its UI.  Seed
    # the repository defaults once, then let the editor own its live settings.
    home-manager.users.${config.user.name}.home.activation.vscodiumSettings =
      inputs.home-manager.lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        vscodium_user_dir="$XDG_CONFIG_HOME/VSCodium/User"
        vscodium_defaults=${escapeShellArg "${configDir}/vscode"}

        $DRY_RUN_CMD mkdir -p "$vscodium_user_dir"
        for vscodium_file in settings.json tasks.json; do
          vscodium_target="$vscodium_user_dir/$vscodium_file"
          if [ -L "$vscodium_target" ]; then
            $DRY_RUN_CMD rm "$vscodium_target"
            $DRY_RUN_CMD cp "$vscodium_defaults/$vscodium_file" "$vscodium_target"
          elif [ ! -e "$vscodium_target" ]; then
            $DRY_RUN_CMD cp "$vscodium_defaults/$vscodium_file" "$vscodium_target"
          fi
        done
      '';
  };
}
