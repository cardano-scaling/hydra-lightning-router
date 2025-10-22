{
  description = "hydra-lightning-router";

  inputs = {
    cardano-node.url = "github:IntersectMBO/cardano-node/10.4.1";
    files.url = "github:mightyiam/files";
    flake-parts.url = "github:hercules-ci/flake-parts";
    horizon.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-hydra";
    htlc.url = "github:cardano-scaling/htlc";
    hydra-coding-standards = {
      url = "github:cardano-scaling/hydra-coding-standards/0.7.1";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };
    hydra.url = "github:cardano-scaling/hydra/0f22b663c46db4fa45e0f3dd2bd96b1f03cfa30e";
    import-tree.url = "github:vic/import-tree";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (inputs.import-tree ./nix);

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cardano-scaling.cachix.org"
      "https://horizon.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "cardano-scaling.cachix.org-1:QNK4nFrowZ/aIJMCBsE35m+O70fV6eewsBNdQnCSMKA="
      "horizon.cachix.org-1:MeEEDRhRZTgv/FFGCv3479/dmJDfJ82G6kfUDxMSAw0="
    ];
    allow-import-from-derivation = true;
  };

}
