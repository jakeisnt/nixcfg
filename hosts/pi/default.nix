{ config, pkgs, lib, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ../personal.nix
  ];

  environment.systemPackages = with pkgs; [
    networkmanager
    raspberrypi-tools
  ];

  networking.hostName = "pi";

  # Preserve space by sacrificing documentation and history
  documentation.nixos.enable = false;

  modules = {
  	editors = {
		default = "nvim";
		vim.enable = true;
	};
	shell = {
		zsh.enable = true;
		ranger.enable = true;
		git.enable = true;
	};
  	services = {
		ssh.enable = true;
	};
  };

  programs.ssh.startAgent = true;
  services.openssh.startWhenNeeded = true;
  networking.networkmanager.enable = true;
}

