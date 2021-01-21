{ stdenv, fetchFromGitHub, meson, ninja, pkg-config, wayland, libGL
, wayland-protocols, libinput, libxkbcommon, pixman, xcbutilwm, libX11, libcap
, xcbutilimage, xcbutilerrors, mesa, libpng, ffmpeg }:

stdenv.mkDerivation {
  pname = "wlroots";
  # version = "0.11.0";

  src = fetchFromGitHub {
    owner = "swaywm";
    repo = "wlroots";
    rev = "7c995b78b2f74a0a2e4c96c6be1736702d3733c7";
    sha256 = "00hzg46xrnmc4k0nx8xmzsgqsjx70kxp5rfp6gfx142nrkwhzdmm";
  };

  # $out for the library and $examples for the example programs (in examples):
  outputs = [ "out" "examples" ];

  nativeBuildInputs = [ meson ninja pkg-config wayland ];

  buildInputs = [
    libGL
    wayland
    wayland-protocols
    libinput
    libxkbcommon
    pixman
    xcbutilwm
    libX11
    libcap
    xcbutilimage
    xcbutilerrors
    mesa
    libpng
    ffmpeg
  ];

  mesonFlags = [ "-Dlogind-provider=systemd" ];

  postFixup = ''
    # Install ALL example programs to $examples:
    # screencopy dmabuf-capture input-inhibitor layer-shell idle-inhibit idle
    # screenshot output-layout multi-pointer rotation tablet touch pointer
    # simple
    mkdir -p $examples/bin
    cd ./examples
    for binary in $(find . -executable -type f -printf '%P\n' | grep -vE '\.so'); do
      cp "$binary" "$examples/bin/wlroots-$binary"
    done
  '';

  meta = with stdenv.lib; {
    description = "A modular Wayland compositor library";
    longDescription = ''
      Pluggable, composable, unopinionated modules for building a Wayland
      compositor; or about 50,000 lines of code you were going to write anyway.
    '';
    inherit (src.meta) homepage;
    license = licenses.mit;
    platforms = platforms.linux;
    maintainers = with maintainers; [ primeos jakeisnt ];
  };
}
