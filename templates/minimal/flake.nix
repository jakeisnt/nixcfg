{
  description = "A gross nixos config.";

  inputs = { nixcfg.url = "github:jakeisnt/nixcfg"; };

  outputs = inputs@{ nixcfg, ... }: {
    nixosConfigurations = nixcfg.lib.mapHosts ./hosts {
      imports = [
        # If this is a linode machine
        # "${dotfiles}/hosts/linode.nix"
      ];
    };
  };
}
