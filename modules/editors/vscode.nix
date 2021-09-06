# I don't actively use VSCode, but it's good to have a configuration handy
# because it's become the industry standard for text editors.
#
# It was a pain to get these hashes working.
# I retrieved the real URL from https://github.com/NixOS/nixpkgs/blob/master/pkgs/misc/vscode-extensions/mktplcExtRefToFetchArgs.nix
# and wrote a quick bash script to fetch the hash provided the url - go check it out in the bin/ folder

{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors.vscode;
  extensions = with pkgs.vscode-extensions;
    (
      [ bbenoist.Nix vscodevim.vim ]
      ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
        {
          name = "vscode-wakatime";
          publisher = "WakaTime";
          version = "5.0.1";
          sha256 = "0wahh7kmrdzcrcp2lj71qxsxbmgdq97k60aw2jxxqlmdfy407nnh";
        }
        {
          name = "nord-visual-studio-code";
          publisher = "arcticicestudio";
          version = "0.15.0";
          sha256 = "066rqj8sf910n71g5njbp5g8advzqkd3g2lsg12wai902735i78c";
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
    home.configFile = {
      "VSCodium/User" = {
        source = "${configDir}/vscode";
        recursive = true;
      };
    };
  };
}
