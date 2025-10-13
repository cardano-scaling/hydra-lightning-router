{
  perSystem = { self', lib, ... }: {
    coding.standards.hydra = {
      enable = true;
      haskellPackages = with self'.packages; [
        hydra-lightning-router
      ];
      haskellFormatter = "ormolu";
    };
    weeder.enable = lib.mkForce false;
  };

}
