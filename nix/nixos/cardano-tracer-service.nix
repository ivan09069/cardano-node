{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
with builtins; let
  inherit (types) ints nullOr str;

  cfg = config.services.cardano-tracer;

  configFile =
    if (cfg.configFile != null)
    then cfg.configFile
    else toFile "cardano-tracer-config.json" (toJSON tracerConfig);

  tracerConfig =
    {
      ekgRequestFreq = 1;

      logging = [
        {
          logFormat = "ForHuman";
          logMode = "JournalMode";
          logRoot = "/tmp/cardano-node-logs";
        }
      ];

      network = {
        contents = "/tmp/forwarder.sock";
        tag = "AcceptAt";
      };

      networkMagic = cfg.networkMagic;

      resourceFreq = null;

      rotation = {
        rpFrequencySecs = 15;
        rpKeepFilesNum = 10;
        rpLogLimitBytes = 1000000000;
        rpMaxAgeHours = 24;
      };
    }
    // optionalAttrs cfg.ekgEnable {
      hasEKG = {
        epHost = cfg.ekgHost;
        epPort = cfg.ekgPort;
      };
    }
    // optionalAttrs cfg.prometheusEnable {
      hasPrometheus = {
        epHost = cfg.prometheusHost;
        epPort = cfg.prometheusPort;
      };
    }
    // optionalAttrs cfg.rtviewEnable {
      hasRTView = {
        epHost = cfg.rtviewHost;
        epPort = cfg.rtviewPort;
      };
    };
  # runtimeDir = i : if cfg.runtimeDir i == null then cfg.stateDir i else "${cfg.runDirBase}${removePrefix cfg.runDirBase (cfg.runtimeDir i)}";
  # suffixDir = base: i: "${base}${optionalString (i != 0) "-${toString i}"}";
  # newTopology = i: {
  #   localRoots = map (g: {
  #     accessPoints = map (e: removeAttrs e ["valency"]) g.accessPoints;
  #     advertise = g.advertise or false;
  #     valency = g.valency or (length g.accessPoints);
  #     trustable = g.trustable or false;
  #   }) (cfg.producers ++ (cfg.instanceProducers i));
  #   publicRoots = map (g: {
  #     accessPoints = map (e: removeAttrs e ["valency"]) g.accessPoints;
  #     advertise = g.advertise or false;
  #   }) (cfg.publicProducers ++ (cfg.instancePublicProducers i));
  #   bootstrapPeers = cfg.bootstrapPeers;
  # } // optionalAttrs (cfg.usePeersFromLedgerAfterSlot != null) {
  #   useLedgerAfterSlot = cfg.usePeersFromLedgerAfterSlot;
  # } // optionalAttrs (cfg.peerSnapshotFile i != null) {
  #   peerSnapshotFile = cfg.peerSnapshotFile i;
  # };
  # mkScript = cfg:
  #   let baseConfig =
  #         recursiveUpdate
  #           (cfg.nodeConfig
  #            // (mapAttrs' (era: epoch:
  #              nameValuePair "Test${era}HardForkAtEpoch" epoch
  #            ) cfg.forceHardForks)
  #           // (optionalAttrs cfg.useNewTopology {
  #             EnableP2P = true;
  #             TargetNumberOfRootPeers = cfg.targetNumberOfRootPeers;
  #             TargetNumberOfKnownPeers = cfg.targetNumberOfKnownPeers;
  #             TargetNumberOfEstablishedPeers = cfg.targetNumberOfEstablishedPeers;
  #             TargetNumberOfActivePeers = cfg.targetNumberOfActivePeers;
  #             MaxConcurrencyBulkSync = 2;
  #           })) cfg.extraNodeConfig;
  #       baseInstanceConfig =
  #         i:
  #         if !cfg.useLegacyTracing
  #         then baseConfig //
  #              { ## XXX: remove once legacy tracing is dropped
  #                minSeverity = "Critical";
  #                setupScribes = [];
  #                setupBackends = [];
  #                defaultScribes = [];
  #                defaultBackends = [];
  #                options = {};
  #              }
  #         else baseConfig //
  #              {
  #                UseTraceDispatcher = false;
  #              } //
  #              (optionalAttrs (baseConfig ? hasEKG) {
  #                 hasEKG = baseConfig.hasEKG + i;
  #              }) //
  #              (optionalAttrs (baseConfig ? hasPrometheus) {
  #                hasPrometheus = map (n: if isInt n then n + i else n) baseConfig.hasPrometheus;
  #              });
  #   in i: let
  #   instanceConfig = recursiveUpdate (baseInstanceConfig i) (cfg.extraNodeInstanceConfig i);
  #   nodeConfigFile = if (cfg.nodeConfigFile != null) then cfg.nodeConfigFile
  #     else toFile "config-${toString cfg.nodeId}-${toString i}.json" (toJSON instanceConfig);
  #   utxoLmdbParams = ["--utxos-on-disk"]
  #     ++ optionals (cfg.lmdbDatabasePath i != null)
  #       [ "--utxos-database-path ${cfg.lmdbDatabasePath i}"
  #       ];
  #   cmd = filter (x: x != "") [
  #     "${cfg.executable} run"
  #     "--config ${nodeConfigFile}"
  #     "--database-path ${instanceDbPath}"
  #     "--topology ${topology i}"
  #   ] ++ optionals (!cfg.systemdSocketActivation) ([
  #     "--host-addr ${cfg.hostAddr}"
  #     "--port ${if (cfg.shareIpv4port || cfg.shareIpv6port) then toString cfg.port else toString (cfg.port + i)}"
  #     "--socket-path ${cfg.socketPath i}"
  #   ] ++ optionals (cfg.ipv6HostAddr i != null) [
  #     "--host-ipv6-addr ${cfg.ipv6HostAddr i}"
  #   ]) ++ optionals (cfg.tracerSocketPathAccept i != null) [
  #     "--tracer-socket-path-accept ${cfg.tracerSocketPathAccept i}"
  #   ] ++ optionals (cfg.tracerSocketPathConnect i != null) [
  #     "--tracer-socket-path-connect ${cfg.tracerSocketPathConnect i}"
  #   ] ++ optionals (cfg.withUtxoHdLmdb i) utxoLmdbParams
  #     ++ consensusParams.${cfg.nodeConfig.Protocol} ++ cfg.extraArgs ++ cfg.rtsArgs;
  #   in ''
  #     echo "Starting: ${concatStringsSep "\"\n   echo \"" cmd}"
  #     echo "..or, once again, in a single line:"
  #     echo "${toString cmd}"
  #     ${optionalString (i > 0) ''
  #     # If exist copy state from existing instance instead of syncing from scratch:
  #     if [ ! -d ${instanceDbPath} ] && [ -d ${cfg.databasePath 0} ]; then
  #       echo "Copying existing immutable db from ${cfg.databasePath 0}"
  #       ${pkgs.rsync}/bin/rsync --archive --ignore-errors --exclude 'clean' ${cfg.databasePath 0}/ ${instanceDbPath}/ || true
  #     fi
  #     ''}
  #     ${toString cmd}'';
in {
  options = {
    services.cardano-tracer = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Enable cardano-tracer, a service for logging and monitoring of
          Cardano nodes. After it is connected to the node(s), it periodically
          asks for different information, receives it, and handles it.
        '';
      };

      #####################################
      #                                   #
      # Alphabetical nixos module options #
      #                                   #
      #####################################

      asserts = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to use an executable with asserts enabled.
        '';
      };

      cardanoNodePackages = mkOption {
        type = types.attrs;
        default = pkgs.cardanoNodePackages or (import ../. {inherit (pkgs) system;}).cardanoNodePackages;
        defaultText = "cardano-node packages";
        description = ''
          The cardano-node packages and library that should be used. The main
          use case is for a sharing optimization which reduces eval time when
          cardano node packages are instantiated multiple times.
        '';
      };

      configFile = mkOption {
        type = nullOr str;
        default = null;
        description = ''
          The actual cardano-tracer configuration file. If this option is set
          to null, a default configuration file will be built based on the nix
          options.
        '';
      };

      ekgEnable = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether to enable an EKG http interface for process monitoring.
        '';
      };

      ekgHost = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = ''
          The host to bind if EKG is enabled.
        '';
      };

      ekgPort = mkOption {
        type = types.port;
        default = 12788;
        description = ''
          The port to listen on if EKG is enabled.
        '';
      };

      environment = mkOption {
        type = types.enum (attrNames cfg.environments);
        default = "preview";
        description = ''
          The environment cardano-tracer will connect to.
        '';
      };

      environments = mkOption {
        type = types.attrs;
        default = cfg.cardanoNodePackages.cardanoLib.environments;
        description = ''
          The environments cardano-tracer will possibly utilize.
        '';
      };

      eventlog = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable eventlog profiling.
        '';
      };

      executable = mkOption {
        type = types.str;
        default = "exec ${cfg.package}/bin/cardano-tracer";
        defaultText = "cardano-node";
        description = ''
          The cardano-tracer executable invocation to use.
        '';
      };

      networkMagic = mkOption {
        type = ints.positive;
        default = (fromJSON (readFile cfg.environments.${cfg.environment}.nodeConfig.ShelleyGenesisFile)).networkMagic;
        description = ''
          The network magic of the cardano environment which will be connected
          with cardano-tracer.
        '';
      };

      package = mkOption {
        type = types.package;
        default =
          if (cfg.profiling != "none")
          then cfg.cardanoNodePackages.cardano-tracer.passthru.profiled
          else if cfg.eventlog
          then cfg.cardanoNodePackages.cardano-tracer.passthru.eventlogged
          else if cfg.asserts
          then cfg.cardanoNodePackages.cardano-tracer.passthru.asserted
          else cfg.cardanoNodePackages.cardano-tracer;
        defaultText = "cardano-tracer";
        description = ''
          The cardano-tracer package that should be used.
        '';
      };

      profiling = mkOption {
        type = types.enum [
          "none"
          "space"
          "space-bio"
          "space-closure"
          "space-cost"
          "space-heap"
          "space-module"
          "space-retainer"
          "space-type"
          "time"
          "time-detail"
        ];
        default = "none";
        description = ''
          Haskell profiling types which are available and will be applied to
          the cardano-tracer binary if declared.
        '';
      };

      prometheusEnable = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether to enable a prometheus export of EKG metrics.
        '';
      };

      prometheusHost = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = ''
          The host to bind if prometheus is enabled.
        '';
      };

      prometheusPort = mkOption {
        type = types.port;
        default = 12798;
        description = ''
          The port to listen on if prometheus is enabled.
        '';
      };

      rtviewEnable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable an RTView client.

          As of node release 9.1 this option has no effect unless node was
          built with `-f +rtview`.

          Ref:
          https://github.com/IntersectMBO/cardano-node/pull/5846
        '';
      };

      rtviewHost = mkOption {
        type = types.str;
        default = "127.0.0.1";
        description = ''
          The host to bind if RTView is enabled.
        '';
      };

      rtviewPort = mkOption {
        type = types.port;
        default = 3300;
        description = ''
          The port to listen on if RTView is enabled.
        '';
      };

      # hostAddr = mkOption {
      #   type = types.str;
      #   default = "127.0.0.1";
      #   description = ''
      #     The host address to bind to
      #   '';
      # };

      # ipv6HostAddr = mkOption {
      #   type = funcToOr nullOrStr;
      #   default = _: null;
      #   apply = ip: if (isFunction ip) then ip else _: ip;
      #   description = ''
      #     The ipv6 host address to bind to. Set to null to disable.
      #   '';
      # };

      # additionalListenStream = mkOption {
      #   type = types.functionTo (types.listOf types.str);
      #   default = _: [];
      #   description = ''
      #     List of additional sockets to listen to. Only available with `systemdSocketActivation`.
      #   '';
      # };

      # stateDirBase = mkOption {
      #   type = types.str;
      #   default = "/var/lib/";
      #   description = ''
      #     Base directory to store blockchain data, for each instance.
      #   '';
      # };

      # stateDir = mkOption {
      #   type = funcToOr types.str;
      #   default = "${cfg.stateDirBase}cardano-node";
      #   apply = x : if (isFunction x) then x else i: x;
      #   description = ''
      #     Directory to store blockchain data, for each instance.
      #   '';
      # };

      # runDirBase = mkOption {
      #   type = types.str;
      #   default = "/run/";
      #   description = ''
      #     Base runtime directory, for each instance.
      #   '';
      # };

      # runtimeDir = mkOption {
      #   type = funcToOr nullOrStr;
      #   default = i: ''${cfg.runDirBase}${suffixDir "cardano-node" i}'';
      #   apply = x : if isFunction x then x else if x == null then _: null else "${cfg.runDirBase}${suffixDir "cardano-node" x}";
      #   description = ''
      #     Runtime directory relative to ${cfg.runDirBase}, for each instance
      #   '';
      # };

      # databasePath = mkOption {
      #   type = funcToOr types.str;
      #   default = i : "${cfg.stateDir i}/${cfg.dbPrefix i}";
      #   apply = x : if isFunction x then x else _ : x;
      #   description = ''Node database path, for each instance.'';
      # };

      # lmdbDatabasePath = mkOption {
      #   type = funcToOr nullOrStr;
      #   default = null;
      #   apply = x : if isFunction x then x else if x == null then _: null else _: x;
      #   description = ''
      #     Node UTxO-HD LMDB path for performant disk I/O, for each instance.
      #     This could point to a direct-access SSD, with a specifically created journal-less file system and optimized mount options.
      #   '';
      # };

      # socketPath = mkOption {
      #   type = funcToOr types.str;
      #   default = i : "${runtimeDir i}/node.socket";
      #   apply = x : if isFunction x then x else _ : x;
      #   description = ''Local communication socket path, for each instance.'';
      # };

      # tracerSocketPathAccept = mkOption {
      #   type = funcToOr nullOrStr;
      #   default = null;
      #   apply = x : if isFunction x then x else _ : x;
      #   description = ''
      #     Listen for incoming cardano-tracer connection on a local socket,
      #     for each instance.
      #   '';
      # };

      # tracerSocketPathConnect = mkOption {
      #   type = funcToOr nullOrStr;
      #   default = null;
      #   apply = x : if isFunction x then x else _ : x;
      #   description = ''
      #     Connect to cardano-tracer listening on a local socket,
      #     for each instance.
      #   '';
      # };

      # socketGroup = mkOption {
      #   type = types.str;
      #   default = "cardano-node";
      #   description = ''
      #     systemd socket group owner.
      #     Note: only applies to sockets created by systemd
      #     (ie. when `systemdSocketActivation` is turned on).
      #   '';
      # };

      # systemdSocketActivation = mkOption {
      #   type = types.bool;
      #   default = false;
      #   description = ''Use systemd socket activation'';
      # };

      # extraServiceConfig = mkOption {
      #   type = types.functionTo types.attrs
      #     // {
      #       merge = loc: foldl' (res: def: i: recursiveUpdate (res i) (def.value i)) (i: {});
      #     };
      #   default = i: {};
      #   description = ''
      #     Extra systemd service config (apply to all instances).
      #   '';
      # };

      # extraSocketConfig = mkOption {
      #   type = types.functionTo types.attrs
      #     // {
      #       merge = loc: foldl' (res: def: i: recursiveUpdate (res i) (def.value i)) (i: {});
      #     };
      #   default = i: {};
      #   description = ''
      #     Extra systemd socket config (apply to all instances).
      #   '';
      # };

      # dbPrefix = mkOption {
      #   type = types.either types.str (types.functionTo types.str);
      #   default = suffixDir "db-${cfg.environment}";
      #   apply = x : if isFunction x then x else suffixDir x;
      #   description = ''
      #     Prefix of database directories inside `stateDir`.
      #     (eg. for "db", there will be db-0, etc.).
      #   '';
      # };

      # port = mkOption {
      #   type = types.either types.int types.str;
      #   default = 3001;
      #   description = ''
      #     The port number
      #   '';
      # };

      # shareIpv4port = mkOption {
      #   type = types.bool;
      #   default = cfg.systemdSocketActivation;
      #   description = ''
      #     Should instances on same machine share ipv4 port.
      #     Default: true if systemd activated socket. Otherwise false.
      #     If false use port increments starting from `port`.
      #   '';
      # };

      # shareIpv6port = mkOption {
      #   type = types.bool;
      #   default = cfg.systemdSocketActivation;
      #   description = ''
      #     Should instances on same machine share ipv6 port.
      #     Default: true if systemd activated socket. Otherwise false.
      #     If false use port increments starting from `port`.
      #   '';
      # };

      # nodeId = mkOption {
      #   type = types.int;
      #   default = 0;
      #   description = ''
      #     The ID for this node
      #   '';
      # };

      # publicProducers = mkOption {
      #   type = types.listOf types.attrs;
      #   default = [];
      #   example = [{
      #     accessPoints = [{
      #       address = envConfig.relaysNew;
      #       port = envConfig.edgePort;
      #     }];
      #     advertise = false;
      #   }];
      #   description = ''Routes to public peers. Only used if slot < usePeersFromLedgerAfterSlot'';
      # };

      # instancePublicProducers = mkOption {
      #   type = types.functionTo (types.listOf types.attrs);
      #   default = _: [];
      #   description = ''Routes to public peers. Only used if slot < usePeersFromLedgerAfterSlot and specific to a given instance (when multiple instances are used).'';
      # };

      # producers = mkOption {
      #   type = types.listOf types.attrs;
      #   default = [];
      #   example = [{
      #     accessPoints = [{
      #       address = "127.0.0.1";
      #       port = 3001;
      #     }];
      #     advertise = false;
      #     valency = 1;
      #   }];
      #   description = ''Static routes to local peers.'';
      # };

      # instanceProducers = mkOption {
      #   type = types.functionTo (types.listOf types.attrs);
      #   default = _: [];
      #   description = ''
      #     Static routes to local peers, specific to a given instance (when multiple instances are used).
      #   '';
      # };

      # useNewTopology = mkOption {
      #   type = types.bool;
      #   default = cfg.nodeConfig.EnableP2P or false;
      #   description = ''
      #     Use new, p2p/ledger peers compatible topology.
      #   '';
      # };

      # useLegacyTracing = mkOption {
      #   type = types.bool;
      #   default = true;
      #   description = ''
      #     Use the legacy tracing, based on iohk-monitoring-framework.
      #   '';
      # };

      # usePeersFromLedgerAfterSlot = mkOption {
      #   type = types.nullOr types.int;
      #   default = if cfg.kesKey != null then null
      #     else envConfig.usePeersFromLedgerAfterSlot or null;
      #   description = ''
      #     If set, bootstraps from public roots until it reaches given slot,
      #     then it switches to using the ledger as a source of peers. It maintains a connection to its local roots.
      #     Default to null for block producers.
      #   '';
      # };

      # bootstrapPeers = mkOption {
      #   type = types.nullOr (types.listOf types.attrs);
      #   default = map (e: {address = e.addr; inherit (e) port;}) envConfig.edgeNodes;
      #   description = ''
      #     If set, it will enable bootstrap peers.
      #     To disable, set this to null.
      #     To enable, set this to a list of attributes of address and port, example: [{ address = "addr"; port = 3001; }]
      #   '';
      # };

      # topology = mkOption {
      #   type = types.nullOr (types.either types.str types.path);
      #   default = null;
      #   description = ''
      #     Cluster topology. If not set `producers` array is used to generated topology file.
      #   '';
      # };

      # useSystemdReload = mkOption {
      #   type = types.bool;
      #   default = false;
      #   description = ''
      #     If set, systemd will reload cardano-node service units instead of restarting them
      #     if only the topology file has changed and p2p is in use.

      #     Cardano-node topology files will be stored in /etc as:
      #       /etc/cardano-node/topology-''${toString i}.yaml

      #     Enabling this option will also allow direct topology edits for tests when a full
      #     service re-deployment is not desired.
      #   '';
      # };

      # nodeConfig = mkOption {
      #   type = types.attrs // {
      #     merge = loc: foldl' (res: def: recursiveUpdate res def.value) {};
      #   };
      #   default = envConfig.nodeConfig;
      #   description = ''Internal representation of the config.'';
      # };

      # targetNumberOfRootPeers = mkOption {
      #   type = types.int;
      #   default = cfg.nodeConfig.TargetNumberOfRootPeers or 100;
      #   description = "Limits the maximum number of root peers the node will know about";
      # };

      # targetNumberOfKnownPeers = mkOption {
      #   type = types.int;
      #   default = cfg.nodeConfig.TargetNumberOfKnownPeers or cfg.targetNumberOfRootPeers;
      #   description = ''
      #     Target number for known peers (root peers + peers known through gossip).
      #     Default to targetNumberOfRootPeers.
      #   '';
      # };

      # targetNumberOfEstablishedPeers = mkOption {
      #   type = types.int;
      #   default = cfg.nodeConfig.TargetNumberOfEstablishedPeers
      #     or (cfg.targetNumberOfKnownPeers / 2);
      #   description = ''Number of peers the node will be connected to, but not necessarily following their chain.
      #     Default to half of targetNumberOfKnownPeers.
      #   '';
      # };

      # targetNumberOfActivePeers = mkOption {
      #   type = types.int;
      #   default = cfg.nodeConfig.TargetNumberOfActivePeers or (2 * cfg.targetNumberOfEstablishedPeers / 5);
      #   description = ''Number of peers your node is actively downloading headers and blocks from.
      #     Default to 2/5 of targetNumberOfEstablishedPeers.
      #   '';
      # };

      # extraNodeConfig = mkOption {
      #   type = types.attrs // {
      #     merge = loc: foldl' (res: def: recursiveUpdate res def.value) {};
      #   };
      #   default = {};
      #   description = ''Additional node config.'';
      # };

      # extraNodeInstanceConfig = mkOption {
      #   type = types.functionTo types.attrs
      #     // {
      #       merge = loc: foldl' (res: def: i: recursiveUpdate (res i) (def.value i)) (i: {});
      #     };
      #   default = i: {};
      #   description = ''Additional node config for a particular instance.'';
      # };

      # nodeConfigFile = mkOption {
      #   type = nullOrStr;
      #   default = null;
      #   description = ''Actual configuration file (shell expression).'';
      # };

      # forceHardForks = mkOption {
      #   type = types.attrsOf types.int;
      #   default = {};
      #   description = ''
      #     A developer-oriented dictionary option to force hard forks for given eras at given epochs.  Maps capitalised era names (Shelley, Allegra, Mary, etc.) to hard fork epoch number.
      #     '';
      # };

      # withCardanoTracer = mkOption {
      #   type = types.bool;
      #   default = false;
      # };

      # withUtxoHdLmdb = mkOption {
      #   type = funcToOr types.bool;
      #   default = false;
      #   apply = x: if isFunction x then x else _: x;
      #   description = ''On an UTxO-HD enabled node, the in-memory backend is the default. This activates the on-disk backend (LMDB) instead.'';
      # };

      # extraArgs = mkOption {
      #   type = types.listOf types.str;
      #   default = [];
      #   description = ''Extra CLI args for 'cardano-node'.'';
      # };

      # rts_flags_override = mkOption {
      #   type = types.listOf types.str;
      #   default = [];
      #   description = ''RTS flags override from profile content.'';
      # };

      # rtsArgs = mkOption {
      #   type = types.listOf types.str;
      #   default = [ "-N2" "-I0" "-A16m" "-qg" "-qb" "--disable-delayed-os-memory-return" ];
      #   apply = args: if (args != [] || cfg.profilingArgs != [] || cfg.rts_flags_override != []) then
      #     ["+RTS"] ++ cfg.profilingArgs ++ args ++ cfg.rts_flags_override ++ ["-RTS"]
      #     else [];
      #   description = ''Extra CLI args for 'cardano-node', to be surrounded by "+RTS"/"-RTS"'';
      # };

      # profilingArgs = mkOption {
      #   type = types.listOf types.str;
      #   default = let commonProfilingArgs = ["--machine-readable" "-tcardano-tracer.stats" "-pocardano-tracer"]
      #     ++ optional (cfg.eventlog) "-l";
      #     in if cfg.profiling == "time" then ["-p"] ++ commonProfilingArgs
      #       else if cfg.profiling == "time-detail" then ["-P"] ++ commonProfilingArgs
      #       else if cfg.profiling == "space" then ["-h"] ++ commonProfilingArgs
      #       else if cfg.profiling == "space-cost" then ["-hc"] ++ commonProfilingArgs
      #       else if cfg.profiling == "space-module" then ["-hm"] ++ commonProfilingArgs
      #       else if cfg.profiling == "space-closure" then ["-hd"] ++ commonProfilingArgs
      #       else if cfg.profiling == "space-type" then ["-hy"] ++ commonProfilingArgs
      #       else if cfg.profiling == "space-retainer" then ["-hr"] ++ commonProfilingArgs
      #       else if cfg.profiling == "space-bio" then ["-hb"] ++ commonProfilingArgs
      #       else if cfg.profiling == "space-heap" then ["-hT"] ++ commonProfilingArgs
      #       else [];
      #   description = ''RTS profiling options'';
      # };

      # peerSnapshotFile = mkOption {
      #   type = funcToOr nullOrStr;
      #   default = null;
      #   example = i: "/etc/cardano-node/peer-snapshot-${toString i}.json";
      #   apply = x: if isFunction x then x else _: x;
      #   description = ''
      #     If set, cardano-node will load a peer snapshot file from the declared absolute path.

      #     The peer snapshot file contains a snapshot of big ledger peers taken at some arbitrary slot.
      #     These are the largest pools that cumulatively hold 90% of total stake.

      #     A peer snapshot file can be generated with a `cardano-cli query ledger-peer-snapshot` command.
      #   '';
      # };
    };
  };

  #       description   = "cardano-node node ${toString i} service";
  #       after         = [ "network-online.target" ]
  #         ++ (optional cfg.systemdSocketActivation "${n}.socket")
  #         ++ (optional (cfg.instances > 1) "cardano-node.service");
  #       requires = optional cfg.systemdSocketActivation "${n}.socket"
  #         ++ (optional (cfg.instances > 1) "cardano-node.service");
  #       wants = [ "network-online.target" ];
  #       wantedBy = [ "multi-user.target" ];
  #       partOf = mkIf (cfg.instances > 1) ["cardano-node.service"];
  #       reloadTriggers = mkIf (cfg.useSystemdReload && cfg.useNewTopology) [ (selectTopology i) ];
  #       script = mkScript cfg i;
  #       serviceConfig = {
  #         User = "cardano-node";
  #         Group = "cardano-node";
  #         ExecReload = mkIf (cfg.useSystemdReload && cfg.useNewTopology) "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
  #         Restart = "always";
  #         RuntimeDirectory = mkIf (!cfg.systemdSocketActivation)
  #           (removePrefix cfg.runDirBase (runtimeDir i));
  #         WorkingDirectory = cfg.stateDir i;
  #         # This assumes cfg.stateDirBase is a prefix of cfg.stateDir.
  #         # This is checked as an assertion below.
  #         StateDirectory =  removePrefix cfg.stateDirBase (cfg.stateDir i);
  #         NonBlocking = mkIf cfg.systemdSocketActivation true;
  #         # Time to sleep before restarting a service
  #         RestartSec = 1;
  #       };
  #     } (cfg.extraServiceConfig i));

  config = mkIf cfg.enable {
    systemd.services.cardano-tracer = {
      description = "cardano-tracer service";
      wantedBy = ["multi-user.target"];

      environment.HOME = "/var/lib/cardano-tracer";

      path = [cfg.package];

      # Allow up to 10 failures with 30 second restarts in a 15 minute window
      # before entering failure state and alerting
      startLimitBurst = 10;
      startLimitIntervalSec = 900;

      serviceConfig = {
        # Apply this downstream (ie: cardano-parts if needed or in deployment cluster)
        # MemoryMax = "${toString (1.15 * cfg.totalMaxHeapSizeMiB)}M";

        LimitNOFILE = "65535";

        StateDirectory = "cardano-tracer";
        WorkingDirectory = "/var/lib/cardano-tracer";

        # Ensure quick restarts on any condition
        Restart = "always";
        RestartSec = 30;

        ExecStart = getExe (pkgs.writeShellApplication {
          name = "cardano-tracer";
          text = ''
            cardano-tracer --config ${configFile}

          '';
        });
      };
    };
  };

  # let
  # lmdbPaths = filter (x: x != null) (map (e: cfg.lmdbDatabasePath e) (genList trivial.id cfg.instances));
  # genInstanceConf = f: listToAttrs (if cfg.instances > 1
  #   then genList (i: let n = "cardano-node-${toString i}"; in nameValuePair n (f n i)) cfg.instances
  #   else [ (nameValuePair "cardano-node" (f "cardano-node" 0)) ]); in mkMerge [
  # {
  #   users.groups.cardano-node.gid = 10016;
  #   users.users.cardano-node = {
  #     description = "cardano-node node daemon user";
  #     uid = 10016;
  #     group = "cardano-node";
  #     isSystemUser = true;
  #   };

  #   environment.etc = mkIf cfg.useSystemdReload (foldl'
  #     (acc: i: recursiveUpdate acc {"cardano-node/topology-${toString i}.yaml".source = selectTopology i;}) {}
  #   (range 0 (cfg.instances - 1)));

  #   ## TODO:  use http://hackage.haskell.org/package/systemd for:
  #   ##   1. only declaring success after we perform meaningful init (local state recovery)
  #   ##   2. heartbeat & watchdog functionality
  #   systemd.services = genInstanceConf (n: i: recursiveUpdate {
  #     description   = "cardano-node node ${toString i} service";
  #     after         = [ "network-online.target" ]
  #       ++ (optional cfg.systemdSocketActivation "${n}.socket")
  #       ++ (optional (cfg.instances > 1) "cardano-node.service");
  #     requires = optional cfg.systemdSocketActivation "${n}.socket"
  #       ++ (optional (cfg.instances > 1) "cardano-node.service");
  #     wants = [ "network-online.target" ];
  #     wantedBy = [ "multi-user.target" ];
  #     partOf = mkIf (cfg.instances > 1) ["cardano-node.service"];
  #     reloadTriggers = mkIf (cfg.useSystemdReload && cfg.useNewTopology) [ (selectTopology i) ];
  #     script = mkScript cfg i;
  #     serviceConfig = {
  #       User = "cardano-node";
  #       Group = "cardano-node";
  #       ExecReload = mkIf (cfg.useSystemdReload && cfg.useNewTopology) "${pkgs.coreutils}/bin/kill -HUP $MAINPID";
  #       Restart = "always";
  #       RuntimeDirectory = mkIf (!cfg.systemdSocketActivation)
  #         (removePrefix cfg.runDirBase (runtimeDir i));
  #       WorkingDirectory = cfg.stateDir i;
  #       # This assumes cfg.stateDirBase is a prefix of cfg.stateDir.
  #       # This is checked as an assertion below.
  #       StateDirectory =  removePrefix cfg.stateDirBase (cfg.stateDir i);
  #       NonBlocking = mkIf cfg.systemdSocketActivation true;
  #       # time to sleep before restarting a service
  #       RestartSec = 1;
  #     };
  #   } (cfg.extraServiceConfig i));

  #   systemd.sockets = genInstanceConf (n: i: mkIf cfg.systemdSocketActivation (recursiveUpdate {
  #     description = "Socket of the ${n} service.";
  #     wantedBy = [ "sockets.target" ];
  #     partOf = [ "${n}.service" ];
  #     socketConfig = {
  #       ListenStream = [ "${cfg.hostAddr}:${toString (if cfg.shareIpv4port then cfg.port else cfg.port + i)}" ]
  #         ++ optional (cfg.ipv6HostAddr i != null) "[${cfg.ipv6HostAddr i}]:${toString (if cfg.shareIpv6port then cfg.port else cfg.port + i)}"
  #         ++ (cfg.additionalListenStream i)
  #         ++ [(cfg.socketPath i)];
  #       RuntimeDirectory = removePrefix cfg.runDirBase (cfg.runtimeDir i);
  #       NoDelay = "yes";
  #       ReusePort = "yes";
  #       SocketMode = "0660";
  #       SocketUser = "cardano-node";
  #       SocketGroup = cfg.socketGroup;
  #       FreeBind = "yes";
  #     };
  #   } (cfg.extraSocketConfig i)));
  # }
  # {
  #   # oneshot service start allows to easily control all instances at once.
  #   systemd.services.cardano-node = mkIf (cfg.instances > 1) {
  #     description = "Control all ${toString cfg.instances} at once.";
  #     enable  = true;
  #     wants = genList (i: "cardano-node-${toString i}.service") cfg.instances;
  #     serviceConfig = {
  #       Type = "oneshot";
  #       RemainAfterExit = "yes";
  #       User = "cardano-node";
  #       Group = "cardano-node";
  #       ExecStart = "${pkgs.coreutils}/bin/echo Starting ${toString cfg.instances} cardano-node instances";
  #       WorkingDirectory = cfg.stateDir i;
  #       StateDirectory =  removePrefix cfg.stateDirBase (cfg.stateDir i);
  #     };
  #   };
  # }
  # {
  #   assertions = [
  #       # {
  #       #   assertion = (length lmdbPaths) == (length (lists.unique lmdbPaths));
  #       #   message   = "When configuring multiple LMDB enabled nodes on one instance, lmdbDatabasePath must be unique.";
  #       # }
  #   ];
  # }
  # ]);
}
# pkgs:
# let serviceConfigToJSON =
#       cfg:
#       {
#         inherit (cfg) networkMagic resourceFreq metricsHelp;
#         # loRequestNum = 100;
#         network =
#           if        cfg.acceptingSocket != null
#           then {
#             tag      = "AcceptAt";
#             contents = cfg.acceptingSocket;
#           } else if cfg.connectToSocket != null
#           then {
#             tag      = "ConnectTo";
#             contents = cfg.connectToSocket;
#           } else
#             throw "cardano-tracer-service:  either acceptingSocket or connectToSocket must be provided.";
#         logging = [{
#           inherit (cfg) logRoot;
#
#           logMode   = "FileMode";
#           logFormat = "ForMachine";
#         }];
#         rotation = {
#           rpFrequencySecs = 15;
#           rpKeepFilesNum  = 10;
#           rpLogLimitBytes = 1000000000;
#           rpMaxAgeHours   = 24;
#         } // (cfg.rotation or {});
#
#         hasEKG = {
#           epHost  = "127.0.0.1";
#           epPort  = cfg.ekgPortBase;
#         };
#         ekgRequestFreq = 1;
#         hasPrometheus = {
#           epHost    = "127.0.0.1";
#           epPort    = 3200; ## supervisord.portShiftPrometheus
#         } // (cfg.prometheus or {});
#         # Just an example for metrics compatibility mapping.
#         # An entry means the first entry has the second entry as alias.
#         # The Metrics is then avalable, both with the original and the mapped name.
#         # Only one mapping per message is supported.
#         # metricsComp = {
#         #     "Mempool.TxsInMempool" = "Mempool.TxsInMempool.Mapped";
#         #     "ChainDB.SlotNum" = "ChainDB.SlotNum.Mapped";
#         # };
#       } // pkgs.optionalAttrs ((cfg.RTView or {}) != {})
#       {
#         hasRTView = cfg.RTView;
#       };
# in pkgs.commondefServiceModule
#   (lib: with lib;
#     { svcName = "cardano-tracer";
#       svcDesc = "Cardano trace processor";
#
#       svcPackageSelector =
#         pkgs: ## Local:
#               pkgs.cardanoNodePackages.cardano-tracer
#               ## Imported by another repo, that adds an overlay:
#                 or pkgs.cardano-tracer;
#               ## TODO:  that's actually a bit ugly and could be improved.
#       ## This exe has to be available in the selected package.
#       exeName = "cardano-tracer";
#
#       extraOptionDecls = {
#         ### You can actually change those!
#         networkMagic    = opt    int 764824073 "Network magic (764824073 for Cardano mainnet).";
#         acceptingSocket = mayOpt str           "Socket path: as acceptor.";
#         connectToSocket = mayOpt str           "Socket path: connect to.";
#         logRoot         = opt    str null      "Log storage root directory.";
#         rotation        = opt    attrs {}      "Log rotation overrides: see cardano-tracer documentation.";
#         RTView          = opt    attrs {}      "RTView config overrides: see cardano-tracer documentation.";
#         ekgPortBase     = opt    int 3100      "EKG port base.";
#         ekgRequestFreq  = opt    int 1         "EKG request frequency";
#         prometheus      = opt    attrs {}      "Prometheus overrides: see cardano-tracer documentation.";
#         resourceFreq    = mayOpt int           "Frequency (1/ms) for tracing resource usage.";
#         metricsHelp     = mayOpt str           "JSON file containing metrics help annotations for Prometheus";
#
#         ### Here be dragons, on the other hand..
#         configFile      = mayOpt str
#           "Config file path override -- only set if you know what you're doing. Shudder. Your 'eminence'..";
#         configJSONfn    = opt (functionTo attrs) serviceConfigToJSON
#           "This is NOT meant to be overridden, at all -- we only expose it so it's externally accessible.";
#       };
#
#       configExeArgsFn = cfg: [
#         "--config" (if cfg.configFile != null then cfg.configFile
#                     else "${pkgs.writeText "cardano-tracer-config.json"
#                                            (__toJSON (serviceConfigToJSON cfg))}")
#         ];
#
#       configSystemdExtraConfig = _: {};
#
#       configSystemdExtraServiceConfig =
#         cfg: with cfg; {
#           Type = "exec";
#           User = "cardano-node";
#           Group = "cardano-node";
#           Restart = "always";
#           # RuntimeDirectory = localNodeConf.runtimeDir;
#           # WorkingDirectory = localNodeConf.stateDir;
#         };
#     })

