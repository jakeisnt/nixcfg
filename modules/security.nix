{ ... }:

{
  ## System security tweaks
  boot.tmp.useTmpfs = true;
  boot.tmp.tmpfsSize = "16G";
  boot.tmp.cleanOnBoot = true;

  security.protectKernelImage = true;

  # Fix a security hole in place for backwards compatibility. See desc in
  # nixpkgs/nixos/modules/system/boot/loader/systemd-boot/systemd-boot.nix
  boot.loader.systemd-boot.editor = false;
}
