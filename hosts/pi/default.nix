{ ...}:

{
  imports = [
    ../../arch/pi
  ];

  environment.systemPackages = with pkgs; [
    networkmanager
    raspberrypi-tools
  ];

  networking.hostname = "pi";

  # Preserve space by sacrificing documentation and history
  documentation.nixos.enable = false;
}

