# I don't actively use VSCode, but it's good to have a configuration handy
# because it's become the industry standard for text editors.

{ config, options, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors.vscode;
  extensions = with pkgs.vscode-extensions;
    ([ bbenoist.Nix vscodevim.vim ] ++ (if config.modules.dev.node.enable then
      pkgs.vscode-utils.extensionsFromVscodeMarketplace [
        # {
        # name = "vscode-eslint";
        # publisher = "dbaeumer";
        # version = "2.1.10";
        # sha256 = "0hqfgxzm4vy1qpkp5n6g7rvhzbwhrwgxhx10wwcaxbzqihbvvglf";
        # }
      ]
    else
      [ ]) ++

      (if config.modules.dev.cc.enable then [
        xaver.clang-format
        ms-vscode.cpptools
      ] else
        [ ]) ++

      (if config.modules.dev.rust.enable then
        [ matklad.rust-analyzer ]
      else
        [ ])) ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [{
          name = "remote-ssh-edit";
          publisher = "ms-vscode-remote";
          version = "0.47.2";
          sha256 = "1hp6gjh4xp2m1xlm1jsdzxw9d8frkiidhph6nvl24d0h8z34w49g";
        }];
  vscodium-with-extensions = pkgs.vscode-with-extensions.override {
    vscode = pkgs.vscodium;
    vscodeExtensions = extensions;
  };
in {
  options.modules.editors.vscode = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ vscodium-with-extensions ];
  };
}
