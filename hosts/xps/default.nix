{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../personal.nix
  ];

  networking.hostName = "xps";

  ## Modules
  modules = {
    desktop = {
      # i3.enable = true;
      sway = {
        enable = true;
        fancy = true;
      };
      # gnome.enable = true;
      browsers = {
        default = "firefox";
        firefox.enable = true;
        # brave.enable = true;
        # chrome.enable = true;
        # netsurf.enable = true;
      };
      media = {
        daw.enable = true;
        documents.enable = true;
        graphics.enable = true;
        recording.enable = true;
        spotify.enable = true;
      };
      term = {
        default = "alacritty";
        alacritty.enable = true;
      };
      vm = {
        # virtualbox.enable = true;
      };
    };

    media = {
      mpv.enable = true;
      ncmpcpp.enable = true;
    };

    messengers = {
      rss.enable = true;
      matrix.enable = true;
      signal.enable = true;
      email.enable = true;
      # deltachat.enable = true;
    };
    editors = {
      default = "nvim";
      emacs = {
        enable = true;
        daemon = true;
      };
      vim.enable = true;
      vscode.enable = true;
    };
    dev = {
      node.enable = true;
      cc.enable = true;
      # android.enable = true;
      # rust.enable = true;
      # lua.enable = true;
      # lua.love2d.enable = true;
    };
    hardware = {
      remarkable.enable = true;
      extraHosts = {
        enable = true;
        allowSocial = false;
      };
      audio.enable = true;
      bluetooth.enable = true;
      fs = {
        enable = true;
        ssd.enable = true;
      };
    };
    shell = {
      git.enable = true;
      gnupg = {
        enable = true;
        gui = true;
        cacheTTL = 60480000;
      };
      # pass.enable = true;
      direnv = {
        enable = true;
        preventGC = true;
      };
      # tmux.enable = true;
      lf.enable = true;
      zsh.enable = true;
    };
    services = {
      syncthing.enable = true;
      ssh.enable = true;
      backup.enable = true;
    };
    theme.active = "nordic";
  };

  programs.ssh.startAgent = true;
  services.openssh.startWhenNeeded = true;
  services.getty.autologinUser = "jake";

  user.packages = with pkgs; [
    rnix-lsp
    clojure-lsp
    # TODO: move these into the hash bang of the webcam script?
    gphoto2
    ffmpeg
  ];

  networking = {
    networkmanager = {
      enable = true;

      wifi = {
        powersave = false; # wifi beast mode
        # TODO: switch to IWD once it's ready. It should provide better performance
        # backend = "iwd";
        # we need to do our best to prevent fingerprinting. not working?
        # macAddress = "random";
      };
    };
  };

  # use dnsmasq to cache dns
  # might want to port this over to other things
  services.dnsmasq = {
    enable = true;

    servers = [ "8.8.8.8" "8.8.4.4" ];

    extraConfig = ''
      interface=lo
      bind-interfaces
      listen-address=127.0.0.1
      cache-size=1000

      no-negcache
    '';
  };

  # Approve polkit access for those in wheel group by default
  # security.polkit.extraConfig = ''
  #   polkit.addRule(function(action, subject) {
  #   if (subject.isInGroup("wheel")) {
  #       return polkit.Result.YES;
  #   }
  #   });
  # '';

  # services = {
  #   # detect devices over DNS
  #   avahi.enable = true;
  #   # knot resolver TODO learn more about this
  #   kresd = {
  #     enable = true;
  #     extraConfig = "verbose(true)";
  #   };
  # };

  # automatic firmware update
  services.fwupd.enable = true;
  services.xserver.libinput.enable = true;
  users.users.jake.extraGroups = [ "networkmanager" "sway" "mopidy" ];

  # Select internationalisation properties.
  console = { keyMap = "us"; };
}
