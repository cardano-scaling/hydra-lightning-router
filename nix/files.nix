{ inputs, ... }: {
  perSystem = { pkgs, ... }:
    let
      files = [
        {
          source = "${inputs.htlc}/htlc/plutus.json";
          target = "hydra-lightning-router/plutus.json";
        }
      ];
    in
    {
      files.files = map
        (x:
          {
            path_ = x.target;
            drv = pkgs.runCommand "file-derivation" { } ''
              cp "${x.source}" $out
            '';
          }
        )
        files;
    };
}
