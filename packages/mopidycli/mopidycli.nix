{ lib, pythonPackages }:

pythonPackages.buildPythonApplication rec {
  pname = "mopidycli";
  version = "0.2.0";

  src = builtins.fetchTarball {
    name = pname;
    url = "https://github.com/havardgulldahl/mopidycli/archive/v0.2.0.tar.gz";
    sha256 = "0s2g9gpr7h5df1j2gfk28llachii8kw0xwq9f5hkwr6wrd5ji781";
  };

  doCheck = false;

  propagatedBuildInputs = with pythonPackages; [ jsonrpclib ];

  meta = with lib; { };
}
