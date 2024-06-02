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
          rev = "68aeafe0586d36bc8cafd0a5b1677a63d2932620";
          sha256 = "0j4grd53cwdk3ni05919pcw0ha5cc5w8gj5aprpkvn0vzjj68hmw";
        });

      geoIpKey = builtins.getEnv "GEOIP_KEY";

      geoIpDb = pkgs.stdenv.mkDerivation {
        name = "database";
        src = pkgs.fetchurl {
          url = "https://download.maxmind.com/app/geoip_download?edition_id=GeoLite2-City&license_key=${geoIpKey}&suffix=tar.gz";
          hash = "sha256-EOrTt8s+/C+9rh9VaYnWJSMLwgS4v2GNXroE2sKHoNg=";
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
      networking.firewall.allowedTCPPorts = [ 22 80 443 ];

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

      security.acme.defaults.email = "james@earldouglas.com";
      security.acme.acceptTerms = true;

      services.nginx = {
        enable = true;
        recommendedGzipSettings = true;
        commonHttpConfig = ''
          charset utf-8;
          log_format postdata '$time_local\t$remote_addr\t$request_body';
          limit_req_zone $binary_remote_addr zone=ip:10m rate=1r/s;
          add_header Permissions-Policy "interest-cohort=()";
          add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
        '';
        virtualHosts = {
          "geode.earldouglas.com" = {
            enableACME = true;
            onlySSL = false; # preferred for securitah
            forceSSL = true; # needed for acme?
            locations = {
              "/".extraConfig = ''
                limit_req zone=ip;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                proxy_pass http://localhost:3000;
              '';
            };
          };
        };
      };
    };

}
