self: super: {
  wl-clipboard-x11 = super.stdenv.mkDerivation rec {
    pname = "wl-clipboard-x11";
    version = "5";

    src = super.fetchFromGitHub {
      owner = "brunelli";
      repo = "wl-clipboard-x11";
      rev = "v${version}";
      sha256 = "1y7jv7rps0sdzmm859wn2l8q4pg2x35smcrm7mbfxn5vrga0bslb";
    };

    dontBuild = true;
    dontConfigure = true;
    propagatedBuildInputs = [ super.wl-clipboard ];
    makeFlags = [ "PREFIX=$(out)" ];
  };

  xsel = self.wl-clipboard-x11;
  xclip = self.wl-clipboard-x11;
}
