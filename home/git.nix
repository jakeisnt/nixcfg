{ pkgs, ... }:
{
  home.packages = with pkgs.gitAndTools; [
    git
    gh
  ];

  programs.git = {
    enable = true;
    userName = "Jacob Chvatal";
    userEmail = "jakechvatal@gmail.com";
    aliases = {
      a = "add --all .";
      s = "status";
      c = "commit -m";
      p = "push";
      d = "diff";
      ds = "diff --staged";
      dt = "difftool";
      dts = "difftool --staged";
      r = "reset --hard HEAD";
      m = "merge";
      i = "\"!f(){ git rm -r --cached . && git a; };f\"";
      mt = "mergetool";
      l = "log --graph --decorate --pretty=oneline --abbrev-commit --all";
      z = "archive HEAD -o";
    };
    ignores = [
      ".venv"
      ".idea"
      ".DS_Store"
      ".envrc"
    ];
    extraConfig = {
      credential = {
        helper = "cache";
      };
      pull = {
        ff = "only";
      };
    };
  };
}
