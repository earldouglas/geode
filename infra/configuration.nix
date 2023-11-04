let

  region = "us-east-2";
  accessKeyId = "geode";

in {

  network.description = "Geode";

  resources.ec2KeyPairs.geode-key-pair = {
      inherit region accessKeyId;
    };

  resources.elasticIPs.geode-elastic-ip = {
    inherit region accessKeyId;
    vpc = true;
    name = "geode";
  };

  geode =
    { resources, pkgs, ... }:
    let

      geode =
        import (pkgs.fetchFromGitHub {
          owner = "earldouglas";
          repo = "geode";
          rev = "c7a5cf704d016d45f502d4bad622c649585290c0";
          sha256 = "1jzv0cbq3ids0m7mx3x0m4rqq210bf3q7nz54v7mrnq2hi07np4v";
        });

      geoIpKey = builtins.getEnv "GEOIP_KEY";

      geoIpDb = pkgs.stdenv.mkDerivation {
        name = "database";
        src = pkgs.fetchurl {
          url = "https://download.maxmind.com/app/geoip_download?edition_id=GeoLite2-City&license_key=${geoIpKey}&suffix=tar.gz";
          hash = "sha256-jNvQCwjEnfwo0LLAgjVlfrRXM5gymVSs9NsweH4QXi4=";
        };
        phases = [ "installPhase" ];
        installPhase = ''
          mkdir -p $out
          tar -xzf "$src" --strip=1 -C $out/
        '';
      };

    in {

      # EC2 ############################################################
      deployment = {
        targetEnv = "ec2";
        ec2 = {
          accessKeyId = accessKeyId;
          region = region;
          instanceType = "t3a.nano";
          keyPair = resources.ec2KeyPairs.geode-key-pair;
          ami = "ami-00f27b88d169080ac"; # "ami-033ff64078c59f378";
          ebsInitialRootDiskSize = 12;
          elasticIPv4 = resources.elasticIPs.geode-elastic-ip;
        };
      };

      # GC #############################################################
      nix.gc.automatic = true;
      nix.gc.options = "-d";
      nix.optimise.automatic = true;

      # Disable docs ###################################################
      documentation.enable = false;
      documentation.dev.enable = false;
      documentation.doc.enable = false;
      documentation.info.enable = false;
      documentation.man.enable = false;
      documentation.nixos.enable = false;

      # Security #######################################################
      services.fail2ban.enable = true;
      networking.firewall.allowedTCPPorts = [ 22 3000 ];

      # Service ########################################################
      users.groups.geode = {};

      users.users.geode = {
        name = "geode";
        group = "geode";
        isSystemUser = true;
      };

      systemd.services.geode = {
        description = "geode";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          ExecStart = "${geode}/bin/geode";
          Restart = "always";
          User = "geode";
        };
        environment = {
          GEOIP_DB = "${geoIpDb}/GeoLite2-City.mmdb";
          PORT = "3000";
        };
      };
    };
}
