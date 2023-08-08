{ inputs, pkgs, project, enable }:
let
  mithril-client = inputs.mithril.packages.${pkgs.system}.mithril-client;

  baseVmModule = { lib, modulesPath, ... }: {
    imports = [
      # The qemu-vm NixOS module gives us the `vm` attribute that we will later use (such as
      # ${machine.config.system.build.vm}), and other VM-related settings.
      "${modulesPath}/virtualisation/qemu-vm.nix"
    ];

    # https://github.com/utmapp/UTM/issues/2353
    networking.nameservers = lib.mkIf pkgs.stdenv.isDarwin [ "8.8.8.8" ];

    virtualisation = {
      graphics = false;

      cores = 1;

      # In MB
      diskSize = 10 * 1024;

      # In MB
      memorySize = 8 * 1024;

      # Use the same pkgs as defined by the marconi project
      host = { inherit pkgs; };
    };
  };

  # This module defines the system that we want
  commonModule = { lib, config, modulesPath, ... }: {
    environment.systemPackages = with pkgs; [
      vim
      htop
    ];

    services.prometheus = {
      enable = true;
      port = 9001;

      exporters = {
        node = {
          enable = true;
          enabledCollectors = [ "systemd" ];
          port = 9002;
        };
      };

      globalConfig = {
        scrape_interval = "30s";
      };

      scrapeConfigs = [
        {
          job_name = "marconi";
          static_configs = [{
            targets = [ "10.0.2.2:8090" ];
          }];
        }
        {
          job_name = "systemd";
          static_configs = [{
            targets = [ "127.0.0.1:9002" ];
          }];
        }
      ];
    };

    services.grafana = {
      enable = true;

      settings = {
        server = {
          http_addr = "127.0.0.1";
          http_port = 3000;
        };
        security = {
          admin_user = "admin";
          admin_password = "admin";
        };
      };

      provision = {
        enable = true;
        datasources.settings.datasources = [
          {
            name = "marconi";
            type = "prometheus";
            access = "proxy";
            url = "http://127.0.0.1:${toString config.services.prometheus.port}";
            isDefault = true;
            uid = "marconi-prometheus-datasource";
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

    services.nginx = {
      enable = true;
      recommendedProxySettings = true;
      virtualHosts.localhost.locations = {
        "/" = {
          proxyPass = "http://127.0.0.1:3000";
        };
      };
    };

    environment.etc = {
      "grafana-dashboards/marconi-dashboard.json" = {
        source = inputs.self + /benchmark/marconi-dashboard.json;
        group = "grafana";
        user = "grafana";
      };
    };

    # Open all ports
    networking.firewall = {
      enable = true;
      allowedTCPPorts = [
        22
        80
        config.services.prometheus.port
      ];
    };

    # Root user without password and enabled SSH for playing around
    services.openssh = {
      enable = true;
      settings.PermitRootLogin = "yes";
    };
    users.extraUsers.root.password = "";
    services.getty.autologinUser = "root";

    system.stateVersion = "22.11";
  };

  # This module defines the system that we want
  localModule = { lib, config, modulesPath, ... }: {
    virtualisation = {
      forwardPorts = [
        { from = "host"; host.port = 8080; guest.port = 80; }
        { from = "host"; host.port = 9001; guest.port = 9001; }
        { from = "host"; host.port = 10022; guest.port = 22; }
      ];
    };
  };

  # TODO Doesn't work yet. Need to complete in the future.
  # Currently fails at running the marconi service.
  benchmarkModule = { lib, config, modulesPath, ... }: {
    virtualisation = {
      forwardPorts = [
        { from = "host"; host.port = 8080; guest.port = 80; }
        { from = "host"; host.port = 9001; guest.port = 9001; }
        { from = "host"; host.port = 10022; guest.port = 22; }
      ];
    };

    imports = [
      inputs.cardano-node.nixosModules.cardano-node
    ];

    services.cardano-node = {
      enable = true;
      environment = "preprod";
    };

    systemd.services.cardano-node.serviceConfig = {
      TimeoutSec = "30min";
      Path = [ pkgs.gnutar pkgs.gzip ];
      ExecStartPre =
        let
          GENESIS_VERIFICATION_KEY_PREPROD_URL = "https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey";
          AGGREGATOR_ENDPOINT_PREPROD_URL = "https://aggregator.release-preprod.api.mithril.network/aggregator";
          SNAPSHOT_DIGEST_PREPROD_URL = "${AGGREGATOR_ENDPOINT_PREPROD_URL}/artifact/snapshots";
        in
        ''
          ${pkgs.bash}/bin/bash -c "${toString [
              ''sleep 10 && if [ ! -d ${config.services.cardano-node.databasePath 0} ] && [ ! -f ${config.services.cardano-node.stateDir 0}/snapshot.tar.gz ]; then''
              ''export GENESIS_VERIFICATION_KEY=$(${pkgs.wget}/bin/wget -q -O - ${GENESIS_VERIFICATION_KEY_PREPROD_URL}) &&''
              ''export AGGREGATOR_ENDPOINT=${AGGREGATOR_ENDPOINT_PREPROD_URL} &&''
              ''export SNAPSHOT_DIGEST=$(${pkgs.curl}/bin/curl -sL ${SNAPSHOT_DIGEST_PREPROD_URL} | ${pkgs.jq}/bin/jq -r '.[0].digest') &&''
              ''echo $GENESIS_VERIFICATION_KEY &&''
              ''echo $AGGREGATOR_ENDPOINT &&''
              ''echo $SNAPSHOT_DIGEST &&''
              ''${mithril-client}/bin/mithril-client snapshot download --download-dir ${config.services.cardano-node.stateDir 0} $SNAPSHOT_DIGEST &&''
              ''mv ${config.services.cardano-node.stateDir 0}/snapshot*.tar.gz ${config.services.cardano-node.stateDir 0}/snapshot.tar.gz &&''
              ''mv ${config.services.cardano-node.stateDir 0}/db ${config.services.cardano-node.databasePath 0};''
              ''elif [ ! -d ${config.services.cardano-node.databasePath 0} ] && [ -f ${config.services.cardano-node.stateDir 0}/snapshot.tar.gz ]; then''
              ''mkdir -p ${config.services.cardano-node.databasePath 0} &&''
              ''${pkgs.gzip}/bin/gzip --decompress --keep ${config.services.cardano-node.stateDir 0}/snapshot.tar.gz &&''
              ''${pkgs.gnutar}/bin/tar xvf ${config.services.cardano-node.stateDir 0}/snapshot.tar -C ${config.services.cardano-node.databasePath 0} &&''
              ''rm ${config.services.cardano-node.stateDir 0}/snapshot.tar;''
              ''fi''
            ]}"
        '';
    };

    systemd.services.marconi-sidechain = {
      description = "Sidechain chain-index service";

      serviceConfig = {
        User = "root";
        Group = "root";
        ExecStartPre = ''
          ${pkgs.bash}/bin/bash -c "mkdir -p /var/lib/cardano-node/preprod/marconi-sidechain"
        '';
        ExecStart = ''${pkgs.bash}/bin/bash -c "${project.hsPkgs.marconi-sidechain.components.exes.marconi-sidechain}/bin/marconi-sidechain --testnet-magic 1 -s ${config.services.cardano-node.socketPath 0} --node-config-path ${config.services.cardano-node.stateDir 0}/config-0-0.json -d ${config.services.cardano-node.stateDir 0}/preprod/marconi-sidechain --http-port 8080 --max-indexing-depth"'';
        ConditionPathExists = config.services.cardano-node.socketPath 0;
      };

      wantedBy = [ "multi-user.target" ];
    };
  };

  localMachine = pkgs.nixos [ baseVmModule commonModule localModule ];

  benchmarkMachine = pkgs.nixos [ baseVmModule commonModule benchmarkModule ];
in
{
  inherit enable;
  exec = ''
    NIX_DISK_IMAGE= $(mktemp -u -t nixos.qcow2.XXX)
    export NIX_DISK_IMAGE
    trap 'rm -f $NIX_DISK_IMAGE' EXIT
    if [ -z "$1" ]
    then
      echo "Missing argument. Choose between 'local' or 'benchmark'."
      exit 1
    fi

    case $1 in
      local)
        ${localMachine.config.system.build.vm}/bin/run-nixos-vm
        ;;
      benchmark)
        ${benchmarkMachine.config.system.build.vm}/bin/run-nixos-vm
        ;;
      *)
        echo "Wrong argument. Choose between 'local' or 'benchmark'."
        exit 1
        ;;
    esac
  '';
  description = ''
    Start the benchmarking NixOS VM exposing Grafana dashboards and Prometheus metrics for Marconi.
  '';
  group = "benchmarking";
}
