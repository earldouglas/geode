## About

Geode is a simple Web frontend for [MaxMind][maxmind]'s [GeoIP2
City][geoip2-city] database.

## Setup

Purchase a [*GeoIP2 City*][geoip2-city] database, or grab a free
[*GeoLite2 City*][geolite2] database from MaxMind:

```
$ curl -s "https://download.maxmind.com/app/geoip_download?edition_id=GeoLite2-City&license_key=<LICENSE_KEY>&suffix=tar.gz" | \
  tar -xz
```

## Build, test, and run

Use Nix to build and test:

```
$ nix-build
```

Then run the compiled binary:

```
$ GEOIP_DB=GeoLite2-City.mmdb PORT=3000 ./result/bin/geode
Running geode...
Setting phasers to stun... (port 3000) (ctrl-c to quit)
```

## Usage

Try it out:

```
$ curl localhost:3000/34.213.34.148
{
    "city": "Boardman",
    "continent": "North America",
    "continentCode": "NA",
    "countryCode": "US",
    "countryName": "United States",
    "latitude": 45.8491,
    "longitude": -119.7143,
    "postalCode": "97818",
    "region": "OR",
    "regionName": "Oregon"
}
```

If you make a request to `/` (leaving off any IP address) Geode will
look up your IP address by checking for any `X-Forwarded-For` header
then falling back to the socket address of your client.

[maxmind]: https://www.maxmind.com/
[geoip2-city]: https://www.maxmind.com/en/geoip2-city
[geolite2]: https://dev.maxmind.com/geoip/geoip2/geolite2/
