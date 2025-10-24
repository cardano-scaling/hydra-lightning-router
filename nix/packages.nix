{ inputs, ... }: {
  perSystem = { lib, system, ... }:
    let
      myOverlay = final: _prev: {
        hydra-lightning-router = final.callCabal2nix "hydra-lightning-router" (lib.cleanSource "${inputs.self}/hydra-lightning-router") { };
      };
      legacyPackages = inputs.horizon.legacyPackages.${system}.extend myOverlay;
      pkgs = import inputs.nixpkgs { inherit system; };
    in
    rec {

      devShells.default = legacyPackages.shellFor {
        packages = p: [ p.hydra-lightning-router ];
        buildInputs = [
          legacyPackages.cabal-install
          pkgs.process-compose
          inputs.cardano-node.packages.${system}.cardano-node
          inputs.cardano-node.packages.${system}.cardano-cli
          inputs.hydra.packages.${system}.hydra-node
        ];
        shellHook = ''
          export PATH="${inputs.hydra.packages.${system}.hydra-node}/bin:$PATH"
        '';
      };

      inherit legacyPackages;

      packages = rec {
        inherit (legacyPackages)
          hydra-lightning-router;
        default = packages.hydra-lightning-router;
      };

    };
}
