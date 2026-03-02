{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      eachSystem = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = eachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          ocamlPackages = pkgs.ocamlPackages;
        in
        {
          default = ocamlPackages.buildDunePackage {
            pname = "chasity";
            version = "0.1.0";
            duneVersion = "3";
            src = ./.;

            buildInputs = [
              ocamlPackages.cmdliner
              ocamlPackages.fmt
              ocamlPackages.alcotest
            ];

            strictDeps = true;
          };
        }
      );

      devShells = eachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          ocamlPackages = pkgs.ocamlPackages;
        in
        {
          default = pkgs.mkShell {
            packages = [
              ocamlPackages.ocaml-lsp
              ocamlPackages.ocamlformat
              pkgs.apache-jena
              pkgs.buf
              pkgs.just
            ];

            inputsFrom = [
              self.packages.${system}.default
            ];
          };
        }
      );
    };
}
