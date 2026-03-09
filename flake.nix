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
        let
          raw = ocamlPackages.buildDunePackage {
            pname = "chasity";
            version = "0.1.1";
            duneVersion = "3";
            src = ./.;

            buildInputs = [
              ocamlPackages.cmdliner
              ocamlPackages.fmt
              ocamlPackages.alcotest
            ];

            strictDeps = true;
          };
        in
        {
          inherit raw;
          default =
            pkgs.runCommand "chasity"
              {
                nativeBuildInputs = [ pkgs.makeWrapper ];
              }
              ''
                mkdir -p $out/bin
                cp ${raw}/bin/chasity $out/bin/chasity
                wrapProgram $out/bin/chasity \
                  --prefix PATH : ${
                    pkgs.lib.makeBinPath [
                      pkgs.apache-jena
                      pkgs.buf
                    ]
                  }
              '';
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
              self.packages.${system}.raw
            ];
          };
        }
      );
    };
}
