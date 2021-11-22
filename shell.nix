with import <nixpkgs> {};
let
in mkShell {
  name = "terraform";
  buildInputs = [
    terraform
  ];
}
