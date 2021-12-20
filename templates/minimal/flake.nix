{
  description = "A grossly incandescent nixos config.";

  inputs = { nix-cfg.url = "github:jakeisnt/nix-cfg"; };

  outputs = inputs@{ nix-cfg, ... }: {
    nixosConfigurations = nix-cfg.lib.mapHosts ./hosts {
      imports = [
        # If this is a linode machine
        # "${dotfiles}/hosts/linode.nix"
      ];
    };
  };
}
