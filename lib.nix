{ inputs, lib, pkgs, ... }:

let
  inherit (builtins) attrValues concatLists listToAttrs pathExists readDir;
  inherit (lib) any baseNameOf count escapeShellArg filterAttrs hasPrefix hasSuffix id
    map mapAttrs mapAttrs' mapAttrsToList mkDefault mkOption nameValuePair removeSuffix types;
in
lib.makeExtensible (_: rec {
  # Attribute-set helpers
  attrsToList = attrs:
    mapAttrsToList (name: value: { inherit name value; }) attrs;

  mapFilterAttrs = pred: f: attrs: filterAttrs pred (mapAttrs' f attrs);

  genAttrs' = values: f: listToAttrs (map f values);

  anyAttrs = pred: attrs:
    any (attr: pred attr.name attr.value) (attrsToList attrs);

  countAttrs = pred: attrs:
    count (attr: pred attr.name attr.value) (attrsToList attrs);

  # Module discovery helpers
  mapModules = dir: fn:
    mapFilterAttrs
      (n: v: v != null && !(hasPrefix "_" n))
      (n: v:
        let path = "${toString dir}/${n}"; in
        if v == "directory" && pathExists "${path}/default.nix"
        then nameValuePair n (fn path)
        else if v == "regular" && n != "secrets.nix" && n != "default.nix" && hasSuffix ".nix" n
        then nameValuePair (removeSuffix ".nix" n) (fn path)
        else nameValuePair "" null)
      (readDir dir);

  mapModules' = dir: fn: attrValues (mapModules dir fn);

  mapModulesRec = dir: fn:
    mapFilterAttrs
      (n: v: v != null && !(hasPrefix "_" n))
      (n: v:
        let path = "${toString dir}/${n}"; in
        if v == "directory"
        then nameValuePair n (mapModulesRec path fn)
        else if v == "regular" && n != "default.nix" && hasSuffix ".nix" n
        then nameValuePair (removeSuffix ".nix" n) (fn path)
        else nameValuePair "" null)
      (readDir dir);

  mapModulesRec' = dir: fn:
    let
      dirs = mapAttrsToList
        (name: _: "${dir}/${name}")
        (filterAttrs (name: type: type == "directory" && !(hasPrefix "_" name)) (readDir dir));
      paths = attrValues (mapModules dir id) ++ concatLists (map (dir': mapModulesRec' dir' id) dirs);
    in map fn paths;

  # NixOS and nix-darwin host constructors
  mkHost = path: attrs @ { system ? "x86_64-linux", ... }:
    let hostname = removeSuffix ".nix" (baseNameOf path); in
    lib.nixosSystem {
      inherit system;
      specialArgs = { inherit lib inputs; };
      modules = [
        {
          nixpkgs.pkgs = pkgs;
          networking.hostName = mkDefault hostname;
        }
        (filterAttrs (name: _: name != "system") attrs)
        ./.
        (import path)
      ];
    };

  mapHosts = dir: attrs @ { system ? "x86_64-linux", ... }:
    mapAttrs
      (name: _: mkHost "${toString dir}/${name}" attrs)
      (filterAttrs
        (name: type: type == "directory" && pathExists "${toString dir}/${name}/default.nix")
        (readDir dir));

  mkDarwinHost = path: attrs @ { system ? "aarch64-darwin", ... }:
    let
      hostname = removeSuffix ".nix" (baseNameOf path);
      darwinPkgs = import inputs.nixpkgs-unstable {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          (final: prev: {
            unstable = import inputs.nixpkgs-unstable {
              inherit system;
              config.allowUnfree = true;
            };
            my = {};
          })
        ];
      };
      darwinLib = inputs.nixpkgs-unstable.lib.extend (self: super: {
        my = import ./lib.nix { inputs = inputs; lib = self; pkgs = darwinPkgs; };
      });
    in
    inputs.nix-darwin.lib.darwinSystem {
      inherit system;
      specialArgs = { lib = darwinLib; inherit inputs; };
      modules = [
        {
          nixpkgs.pkgs = darwinPkgs;
          networking.hostName = mkDefault hostname;
        }
        (filterAttrs (name: _: name != "system") attrs)
        ./darwin.nix
        (import path)
      ];
    };

  mapDarwinHosts = dir: attrs @ { system ? "aarch64-darwin", ... }:
    mapModules dir (hostPath: mkDarwinHost hostPath attrs);

  # Option constructors
  mkOpt = type: default: mkOption { inherit type default; };

  mkOpt' = type: default: description:
    mkOption { inherit type default description; };

  mkBoolOpt = default: mkOption {
    inherit default;
    type = types.bool;
    example = true;
  };

  # Paths and Home Manager helpers
  dotFilesDir = toString ./.;
  modulesDir = "${dotFilesDir}/modules";
  configDir = "${dotFilesDir}/config";
  binDir = "${dotFilesDir}/bin";
  themesDir = "${modulesDir}/themes";
  username =
    if pkgs.stdenv.hostPlatform.isDarwin
    then let name = builtins.getEnv "DARWIN_USER"; in if name == "" then "jake" else name
    else "jake";
  homeDir = "/home/${username}";
  darwinHomeDir = "/Users/${username}";

  mkOutOfStoreSymlink = path:
    let pathStr = toString path; in
    pkgs.runCommandLocal (baseNameOf pathStr) {} "ln -s ${escapeShellArg pathStr} $out";

  # Build helpers
  toCSSFile = file:
    let
      fileName = baseNameOf file;
      compiledStyles = pkgs.runCommand "compileScssFile" { buildInputs = [ pkgs.sass ]; } ''
        mkdir "$out"
        scss --sourcekkmap=none --no-cache --style compressed --default-encoding utf-8 "${file}" >>"$out/${fileName}.css"
      '';
    in "${compiledStyles}/${fileName}";

  toFilteredImage = imageFile: options:
    let
      result = "result.png";
      filteredImage = pkgs.runCommand "filterWallpaper" { buildInputs = [ pkgs.imagemagick ]; } ''
        mkdir "$out"
        convert ${options} ${imageFile} $out/${result}
      '';
    in "${filteredImage}/${result}";
})
