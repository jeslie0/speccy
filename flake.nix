{
  description = "A flake for building PureScript projects.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    ps-overlay.url = "github:thomashoneyman/purescript-overlay";
    mkSpagoDerivation.url = "/home/james/workspaces/PureScript/mkSpagoDerivation";
    # mkSpagoDerivation.url = "github:jeslie0/mkSpagoDerivation";
    closure-compiler.url = "github:jeslie0/closure-compiler-acocr";
  };

  outputs = { self, nixpkgs, ps-overlay, mkSpagoDerivation, closure-compiler }:
    let
      supportedSystems =
        [ "aarch64-linux" "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];

      forAllSystems =
        nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
          overlays = [ ps-overlay.overlays.default
                       mkSpagoDerivation.overlays.default
                     ];
        });

      patternfly-v5 = system:
        nixpkgsFor.${system}.stdenvNoCC.mkDerivation rec {
          pname =
            "patternfly";

          version =
            "5.1.0";

          src =
            builtins.fetchTarball {
              url = "https://registry.npmjs.org/@patternfly/patternfly/-/patternfly-${version}.tgz";
              sha256 = "sha256:1cdcd9z263wjvh9jadrry8b4zndc5m1vkxb4hz04xhj69fz9dyli";
            };

          installPhase =
            "mkdir -p $out/patternfly; mv * $out/patternfly";
          };

      dependencies =
        system:
        with nixpkgsFor.${system};
          [ purs-unstable
            purs-backend-es
            esbuild
            spago-unstable
            closure-compiler.packages.${system}.default
            nodePackages.uglify-js
          ];

        packageJson =
          builtins.fromJSON (builtins.readFile ./package.json);

        devDeps = system:
          nixpkgsFor.${system}.buildNpmPackage {
            name =
              packageJson.name;

            src =
              nixpkgsFor.${system}.lib.sources.cleanSourceWith {
                src =
                  ./.;

                filter =
                  path: type: builtins.baseNameOf path == "package.json" ||builtins.baseNameOf path == "package-lock.json";
              };


            dontNpmBuild =
              true;

            npmDepsHash =
              "sha256-AOuluSr60Q0Ru6SmKJA1hJ8s9sdSBC9l2Yp2Z39gH1I=";
          };
      in
        {
          packages =
            forAllSystems (system:
              let
                pkgs =
                  nixpkgsFor.${system};

                packageName =
                  (pkgs.fromYAML
                    (builtins.readFile "${./.}/spago.yaml")).package.name;
              in
                {
                  default =
                    pkgs.mkSpagoDerivation {
                      pname =
                        packageName;

                      version =
                        "0.1.0";

                      src =
                        ./.;

                      nativeBuildInputs =
                        dependencies system;

                      patches =
                        [ ./patches/backend.patch ];

                      buildPhase =
                        ''
                        spago build && \
                        purs-backend-es bundle-app --minify --int-tags --to main.es.js && \
                        closure-compiler-acocr -O SIMPLE --assume_function_wrapper true --isolation_mode IIFE --emit_use_strict --js_output_file main.cc.js main.es.js && \
                        uglifyjs --compress --mangle --output main.min.js main.cc.js
                        '';

                      installPhase =
                        ''
                        mkdir $out
                        cp -r public/* $out
                        cp -r ${patternfly-v5 system}/patternfly $out/css/patternfly
                        cp main.min.js $out/js/main.min.js
                        rm $out/dev.html
                        rm $out/dev.js
                        '';
                    };
              }
            );

          devShell =
            forAllSystems (system:
              let
                pkgs =
                  nixpkgsFor.${system};
              in
                pkgs.mkShell {
                  inputsFrom = [
                    # self.packages.${system}.default
                  ]; # Include build inputs from packages in
                  # this list

                  shellHook = ''
                    echo "Run the following command to get the development dependencies in place using nix:

ln -s ${devDeps system}/lib/node_modules/${packageJson.name}/node_modules $(git rev-parse --show-toplevel)/node_modules

Then run \"npm start\" to start the development server.
Run \"spago init\" then uncomment the default package from the shell inputs.
Make sure \"dist\" and \".parcel-cache\" are added to the .gitignore file."
                  '';

                  packages = with pkgs;
                    [ purescript-language-server
                      purs-tidy
                      nodejs
                    ] ++ (dependencies system); # Extra packages to go in the shell
                }
            );
        };
}
