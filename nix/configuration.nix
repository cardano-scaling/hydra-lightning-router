{ inputs, ... }: {
  imports = [
    inputs.files.flakeModules.default
    inputs.hydra-coding-standards.flakeModule
    inputs.process-compose-flake.flakeModule
  ];

}
