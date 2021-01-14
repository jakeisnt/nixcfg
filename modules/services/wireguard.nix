{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.wireguard;
in {
  options.modules.services.wireguard = {
    server = {
      enable = mkBoolOpt false;
    };
    client = {
      enable = mkBoolOpt false;
    }
  };

  config = mkMerge [
    (mkIf cfg.client.enable {
      networking.wg-quick.interfaces = {
        wg0 = {
          address = [ "10.0.0.2/24" "fcd9:281f:04d7:9ee9::2/64" ];
          dns = [ "10.0.0.1" "fcd9:281f:04d7:9ee9::1" ];
          privateKey = secrets.xps.wireguard.privateKey;

          peers = [{
            publicKey = secrets.vultr.wireguard.publicKey;
            presharedKeyFile = "/root/wireguard-keys/preshared_from_peer0_key";
            allowedIPs = [ "0.0.0.0/0" "::/0" ];
            endpoint = "${secrets.domain}:51820";
            persistentKeepalive = 25;
          }];
        };
      };

    })
    (mkIf cfg.server.enable {
      # enable NAT
      networking = {
        nat = {
          enable = true;
          externalInterface = "eth0";
          internalInterfaces = [ "wg0" ];
        };
        firewall = {
          allowedTCPPorts = [ 53 ];
          allowedUDPPorts = [ 53 51820 ];
        };
      };

      networking.wg-quick.interfaces = {
        # "wg0" is the network interface name. You can name the interface arbitrarily.
        wg0 = {
          # Determines the IP/IPv6 address and subnet of the client's end of the tunnel interface
          address = [ "10.0.0.1/24" "fdc9:281f:04d7:9ee9::1/64" ];
          listenPort = 51820;
          privateKey = secrets.vultr.wireguard.privateKey;

          # This allows the wireguard server to route your traffic to the internet and hence be like a VPN
          postUp = ''
            ${pkgs.iptables}/bin/iptables -A FORWARD -i wg0 -j ACCEPT
            ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.0.0.1/24 -o eth0 -j MASQUERADE
            ${pkgs.iptables}/bin/ip6tables -A FORWARD -i wg0 -j ACCEPT
            ${pkgs.iptables}/bin/ip6tables -t nat -A POSTROUTING -s fdc9:281f:04d7:9ee9::1/64 -o eth0 -j MASQUERADE
          '';

          # Undo the above
          preDown = ''
            ${pkgs.iptables}/bin/iptables -D FORWARD -i wg0 -j ACCEPT
            ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.0.0.1/24 -o eth0 -j MASQUERADE
            ${pkgs.iptables}/bin/ip6tables -D FORWARD -i wg0 -j ACCEPT
            ${pkgs.iptables}/bin/ip6tables -t nat -D POSTROUTING -s fdc9:281f:04d7:9ee9::1/64 -o eth0 -j MASQUERADE
          '';

          peers = [{ # peer0
            publicKey = secrets.xps.wireguard.publicKey;
            presharedKeyFile = "/root/wireguard-keys/preshared_from_peer0_key";
            allowedIPs = [ "10.0.0.2/32" "fdc9:281f:04d7:9ee9::2/128" ];
          }
          # More peers can be added here.
            ];
        };
      };
    })
  ];
}
