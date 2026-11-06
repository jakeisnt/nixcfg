final: prev: {
  nixosOptionsDoc = args: prev.nixosOptionsDoc (args // {
    transformOptions = opt:
      let
        transformed = (args.transformOptions or (x: x)) opt;
        linkDeclaration = declaration:
          let
            match = if builtins.isString declaration then
              builtins.match "/nix/store/[^/]+/(modules/generic/.*)" declaration
            else null;
          in if match == null then declaration else {
            name = builtins.head match;
            url = "https://github.com/NixOS/nixpkgs/blob/nixos-${prev.lib.versions.majorMinor prev.lib.version}/${builtins.head match}";
          };
      in transformed // {
        # These are source locations for documentation, not build dependencies.
        # Match the generic module subtree because lazy trees and materialized
        # Nixpkgs sources can have different store roots.
        declarations = map linkDeclaration transformed.declarations;
      };
  });
}
