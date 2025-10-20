{ inputs, ... }: {
  perSystem = { lib, system, ... }:
    let
      myOverlay = final: _prev: {
        hydra-lightning-router = final.callCabal2nix "hydra-lightning-router" (lib.cleanSource "${inputs.self}/hydra-lightning-router") { };
      };
      legacyPackages = inputs.horizon.legacyPackages.${system}.extend myOverlay;
    in
    rec {

      devShells.default = legacyPackages.shellFor {
        packages = p: [ p.hydra-lightning-router ];
        buildInputs = [
          legacyPackages.cabal-install
          inputs.cardano-node.packages.${system}.cardano-node
          inputs.cardano-node.packages.${system}.cardano-cli
          inputs.hydra.packages.${system}.hydra-node
        ];
      };

      inherit legacyPackages;

      packages = rec {
        inherit (legacyPackages)
          hydra-lightning-router;
        default = packages.hydra-lightning-router;
      };

    };
}
