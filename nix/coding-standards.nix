{
  perSystem = { self', lib, ... }: {
    coding.standards.hydra = {
      enable = true;
      haskellPackages = with self'.packages; [
        hydra-lightning-router
      ];
    };
    weeder.enable = lib.mkForce false;
  };

}
