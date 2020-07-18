let
  freshpkgs = import <nixpkgs> {};
  inherit (freshpkgs.lib) optionals optionalAttrs;
  inherit (freshpkgs.hostPlatform) isLinux isMacOS;
in
{
  justLinux = a: optionals isLinux a;
  justLinuxAttrs = a: optionalAttrs isLinux a;
  justMacOS = a: optionals isMacOS a;
  justMacOSAttrs = a: optionalAttrs isMacOS a;
}
