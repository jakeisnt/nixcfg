{ config, lib, pkgs, ... }:

{
  imports =
    let nur-no-pkgs =
          import(
            builtins.fetchTarball
              "https://github.com/nix-community/NUR/archive/master.tar.gz"
          ) {};
        in
    [
      ./hardware-configuration.nix
      nur-no-pkgs.repos.mmilata.modules.jitsi-meet
    ];


  environment.systemPackages = with pkgs; [
    riot-web
  ];

  services.jitsi-meet = {
    enable = true;
    hostName = "jacob.chvatal.com";
    videobridge.openFirewall = true;
  };

  services.coturn = {
    enable = true;
    use-auth-secret = true;
    # static-auth-secret =  TODO: nix-shell -p pwgen --command "pwgen -s 64 1"
    realm = "turn.chvatal.com";
    no-tcp-relay = true;
    no-tls = true;
    no-dtls = true;
    extraConfig = ''
      user-quota=12
      total-quota=1200
      denied-peer-ip=10.0.0.0-10.255.255.255
      denied-peer-ip=192.168.0.0-192.268.255.255
      denied-peer-ip=172.16.0.0-172.16.255.255
      allowed-peer-ip=192.168.191.127 
    '';
  };

  nixpkgs.overlays = [
    (self: super: {
      riot-web = super.riot-web.override {
        conf = {
          default_server_config = {
            "m.homeserver" = {
              "base_url" = "https://matrix.chvatal.com";
              "server_name" = "matrix.chvatal.com";
            };
            "m.identity_server" = {
              "base_url" = "https://vector.im";
            };
          };
          jitsi.preferredDomain = "jitsi.chvatal.com";
        };
      };
    })
  ];

  services.syncthing = {
    enable = true;
    openDefaultPorts = true;
    systemService = true;
    user = "jake";
    group = "wheel";
    dataDir = "/home/jake";
  };

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "vps"; # Define your hostname.
  
  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp2s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "latarcyrheb-sun32";
    keyMap = "us";
  };


  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  #   pinentryFlavor = "gnome3";
  # };


  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  networking.networkmanager.enable = true;

  users.users.jake = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "video" ]; # Enable ‘sudo’ for the user.
  };

  system.stateVersion = "20.03";

  networking.firewall = {
    allowedUDPPorts = [ 5349 5350 ];
    allowedTCPPorts = [ 80 443 3478 3479 ];
  };

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;

    virtualHosts = {
      "matrix.chvatal.com" = {
        forceSSL = true;
        enableACME = true;
        locations."/" = {
          proxyPass = "http://localhost:8008";
        };
      };

      "riot.chvatal.com" = {
        forceSSL = true;
        enableACME = true;
        locations."/" = {
          root = pkgs.riot-web;
        };
      };

      ${config.services.jitsi-meet.hostName} = {
        enableACME = true;
        forceSSL = true;
      };
    };
  };

  services.matrix-synapse = {
    enable = true;
    server_name = "matrix.chvatal.com";
    enable_metrics = true;
    enable_registration = true;
    database_type = "psycopg2";

    database_args = {
      password = "synapse";
    };

    listeners = [
      {
        port = 8008;
        tls = false;
        resources = [
          {
            compress = true;
            names = ["client" "webclient" "federation"];
          }
        ];
      }
    ];

    turn_uris = [
      "turn:turn.chvatal.com:3478?transport=udp"
      "turn:turn.chvatal.com:3478?transport=tcp"
    ];
    turn_shared_secret = config.services.coturn.static-auth-secret;
  };

  services.postgresql = {
    enable = true;
    initialScript= pkgs.writeText "synapse-init.sql" ''
CREATE ROLE "matrix-synapse" with lOGIN PASSWORD 'synapse';
CREATE DATABASE "matrix-synapse" WITH OWNER "matrix-synapse"
       TEMPLATE template0
       LC_COLLATE = "C"
       LC_CTYPE = "C"

       '';
  };
}
