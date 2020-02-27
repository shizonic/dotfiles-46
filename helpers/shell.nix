with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "node";
  buildInputs = [
    nodejs
  ];
  shellHook = ''
        export PATH="$PWD/node_modules/.bin/:$PATH"
        npm install javascript-typescript-langserver
    '';
}
