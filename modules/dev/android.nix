{ config, options, lib, pkgs, ... }:
# https://nixos.wiki/wiki/Android
with lib;
with lib.my;
let cfg = config.modules.dev.android;
    androidSdk = pkgs.androidenv.androidPkgs_9_0.androidsdk;
    platformTools = pkgs.androidenv.androidPkgs_9_0.platform-tools;
in {
  options.modules.dev.android = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      android-studio
      androidSdk
      platformTools
      glibc
      openjdk8
    ];
    programs.adb.enable = true;
    user.extraGroups = ["adbusers"];
    services.udev.packages = [
      pkgs.android-udev-rules
    ];
    environment.variables.ANDROID_JAVA_HOME="${pkgs.openjdk8.home}";
    environment.variables.ANDROID_SDK_ROOT="${androidSdk}";
    environment.variables.GRADLE_OPTS = "-Dorg.gradle.project.android.aapt2FromMavenOverride=${androidSdk}/libexec/android-sdk/build-tools/28.0.3/aapt2";

    env.ANDROID_JAVA_HOME="${pkgs.openjdk8.home}";
    env.ANDROID_SDK_ROOT="${androidSdk}";

    # override the aapt2 that gradle uses with the nix-shipped version
    env.GRADLE_OPTS = "-Dorg.gradle.project.android.aapt2FromMavenOverride=${androidSdk}/libexec/android-sdk/build-tools/28.0.3/aapt2";
  };
}
