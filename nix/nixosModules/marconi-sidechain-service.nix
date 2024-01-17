{ config, lib, pkgs, ... }:

let
  cfg = config.services.marconi-sidechain;

  networkMagicOpt =
    if cfg.network == "mainnet"
    then "--mainnet"
    else if cfg.network == "preprod"
    then "--testnet-magic 1"
    else if cfg.network == "preview"
    then "--testnet-magic 2"
    else if cfg.network == "sanchonet"
    then "--testnet-magic 4"
    else "--testnet-magic ${builtins.toString cfg.network}";

  cmd = builtins.toString (builtins.filter (x: x != "") [
    "${cfg.executable} "
    # Mandatory CLI params
    "--socket-path ${cfg.nodeSocketPath}"
    "${networkMagicOpt}"
    "--node-config-path ${cfg.nodeConfigFile}"
    "--db-dir ${cfg.databaseDir}"
    # Optional  CLI params
    "--initial-retry-time ${builtins.toString cfg.initialRetryTime}"
    "--max-retry-time ${builtins.toString cfg.maxRetryTime}"
    "--http-port ${builtins.toString cfg.port}"
  ]);
in
{
  options = {
    services.marconi-sidechain = {
      enable = lib.mkEnableOption "marconi-sidechain";

      executable = lib.mkOption {
        type = lib.types.str;
        default = "marconi-sidechain";
        example = [ "marconi-sidechain" "nix run github:input-output-hk/marconi#marconi-sidechain-experimental --" ];
        description = "The marconi-sidechain executable invocation to use";
      };

      nodeSocketPath = lib.mkOption {
        type = lib.types.str;
        example = "/var/lib/cardano-node/mainnet/cardano-node.socket";
        description = "Local Cardano node communication socket path";
      };

      network = lib.mkOption {
        type = lib.types.either (lib.types.enum [ "mainnet" "preprod" "preview" "sanchonet" ]) lib.types.int;
        example = [ "mainnet" 1 2 ];
        description = "The Cardano network we want to connect to";
      };

      nodeConfigFile = lib.mkOption {
        type = lib.types.str;
        example = "/var/lib/cardano-node/mainnet/config.json";
        description = "Node configuration file. Mainly used by the EpochState indexer.";
      };

      databaseDir = lib.mkOption {
        type = lib.types.str;
        example = "/var/lib/cardano-node/mainnet/marconi-sidechain";
        description = "Directory containing the SQLite databases created by marconi-sidechain";
      };

      port = lib.mkOption {
        type = lib.types.int;
        default = 8090;
        example = 8090;
        description = "The port number of marconi-sidechain's HTTP interface";
      };

      initialRetryTime = lib.mkOption {
        type = lib.types.int;
        default = 5;
        example = 5;
        description = "Initial time (in seconds) before retry after failing to connect to local node.";
      };

      maxRetryTime = lib.mkOption {
        type = lib.types.int;
        default = 300;
        example = 300;
        description = "Max time (in seconds) allowed after startup for retries. If omitted, then it will retry indefinetely.";
      };

      monitoring = {
        enable = lib.mkEnableOption "marconi-sidechain monitoring";
      };
    };
  };

  config =
    let
      marconiSidechainConfig =
        lib.mkIf cfg.enable (
          {
            systemd.services.marconi-sidechain = {
              description = "Runs marconi-sidechain";

              serviceConfig = {
                User = "root";
                Group = "root";
                ExecStart = "${cmd}";
                Restart = "always";
              };

              wants = [ "network-online.target" ];
              wantedBy = [ "multi-user.target" ];
            };
          });
      monitoringConfig =
        lib.mkIf cfg.monitoring.enable ({
          services.prometheus = {
            scrapeConfigs = [
              {
                job_name = "marconi-sidechain";
                static_configs = [{
                  targets = [ "127.0.0.1:${toString config.services.marconi-sidechain.port}" ];
                }];
              }
            ];
          };

          services.grafana = {
            provision = {
              datasources.settings.datasources = [
                {
                  name = "marconi-sidechain";
                  type = "prometheus";
                  access = "proxy";
                  url = "http://127.0.0.1:${toString config.services.prometheus.port}";
                  uid = "marconi-sidechain-prometheus-datasource";
                }
              ];
              dashboards.settings.providers = [
                {
                  name = "default";
                  type = "file";
                  folder = "";
                  updateIntervalSeconds = 30;
                  options = {
                    path = "/etc/grafana-dashboards";
                  };
                }
              ];
            };
          };

          environment.etc = {
            "grafana-dashboards/marconi-dashboard.json" = {
              source = ./marconi-dashboard.json;
              group = "root";
              user = "root";
            };
          };
        });
    in
    lib.mkMerge [ marconiSidechainConfig monitoringConfig ];
}
