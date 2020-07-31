{ pkgs, ... }: {
  home.packages = with pkgs.gitAndTools; [ git gh ];

  programs.git = {
    enable = true;
    userName = "jakechv";
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
      i = ''"!f(){ git rm -r --cached . && git a; };f"'';
      mt = "mergetool";
      l = "log --graph --decorate --pretty=oneline --abbrev-commit --all";
      z = "archive HEAD -o";
      co = "checkout";
      br = "branch";
    };
    ignores = [ ".venv" ".idea" ".DS_Store" ];
    extraConfig = {
      credential = { helper = "cache"; };
      pull = { ff = "only"; };
    };
  };
}
