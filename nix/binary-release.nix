############################################################################
# Windows release CARDAN~1.ZIP
#
# This bundles up the windows build and its dependencies,
# and sets up the Hydra build artifact.
#
############################################################################

{ pkgs
, version
, exes
, platform
}:

let
  inherit (pkgs) lib;

  name = "cardano-node-${version}-${platform}";

  environments = lib.getAttrs
    [ "mainnet" "preprod" "preview" ]
    pkgs.cardanoLib.environments;


  writeConfig = name: env:
    let
      genesisAttrs = {
        # File references point to the nix store, so we need to rewrite them
        # as relative paths
        ByronGenesisFile =  "byron-genesis.json";
        ShelleyGenesisFile = "shelley-genesis.json";
        AlonzoGenesisFile = "alonzo-genesis.json";
      } // lib.optionalAttrs (env.nodeConfig ? ConwayGenesisFile) {
        ConwayGenesisFile = "conway-genesis.json";
      };
      nodeConfig = pkgs.writeText
        "config.json"
        (builtins.toJSON
          (env.nodeConfig // genesisAttrs));

      nodeConfigBp = pkgs.writeText
        "config-bp.json"
        (builtins.toJSON
          (env.nodeConfigBp // genesisAttrs));

      topologyConfig = pkgs.cardanoLib.mkTopology env;

      # Genesis files are the same for env.nodeConfig and env.nodeConfigBp
      inherit (env.nodeConfig)
        ByronGenesisFile ShelleyGenesisFile AlonzoGenesisFile;
    in
      # Format the node config file and copy the genesis files
      ''
        mkdir -p "share/${name}"
        jq . < "${nodeConfig}" > share/${name}/config.json
        jq . < "${nodeConfigBp}" > share/${name}/config-bp.json
        jq . < "${topologyConfig}" > share/${name}/topology.json
        cp -n --remove-destination -v \
          "${ByronGenesisFile}" \
           share/${name}/byron-genesis.json
        cp -n --remove-destination -v \
          "${ShelleyGenesisFile}"  \
           share/${name}/shelley-genesis.json
        cp -n --remove-destination -v \
          "${AlonzoGenesisFile}" \
           share/${name}/alonzo-genesis.json
        ${lib.optionalString (env.nodeConfig ? ConwayGenesisFile) ''
        cp -n --remove-destination -v \
          "${env.nodeConfig.ConwayGenesisFile}" \
           share/${name}/conway-genesis.json
        ''}
      '';

in pkgs.runCommand name {
    nativeBuildInputs = with pkgs.pkgsBuildBuild; [
      haskellBuildUtils bintools jq nix zip
    ];
  } ''
  mkdir -p $out release/{bin,share}
  cd release

  # note: on windows, we have all the .dlls in the same /bin folder. Thus we will
  #       get the same dlls for each executable multiple times. So we cannot really
  #       use `-n` here, which would warn that we "skipped" some duplicates; and
  #       exit with 1. `-u` on the otherhand will just update as needed.
  cp -u --remove-destination -v ${pkgs.lib.concatMapStringsSep " " (exe: "${exe}/bin/*") exes} ./bin
  chmod -R +w .

  ${lib.pipe environments [
    (lib.mapAttrs writeConfig)
    (lib.mapAttrsToList (_: val: val))
    (lib.concatStringsSep "\n")
  ]}

  ${lib.optionalString (platform == "macos") (lib.concatMapStrings (exe: ''
    rewrite-libs bin ${exe}/bin/*
  '') exes)}

  ${if (platform == "win64")
    then "zip -r $out/${name}.zip ."
    else "tar -czf $out/${name}.tar.gz ."
  }
  dist_file=$(ls $out)
  mkdir -p $out/nix-support
  echo "file binary-dist $out/$dist_file" > $out/nix-support/hydra-build-products
''
