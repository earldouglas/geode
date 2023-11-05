{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import           Data.CaseInsensitive (CI)
import           Data.GeoIP2 (GeoDB)
import           Data.GeoIP2 (GeoResult(..))
import           Data.GeoIP2 (GeoResult)
import           Data.GeoIP2 (Location(..))
import           Data.GeoIP2 (Location)
import           Data.GeoIP2 (findGeoData)
import           Data.GeoIP2 (geoCity)
import           Data.GeoIP2 (geoCityConfidence)
import           Data.GeoIP2 (geoContinent)
import           Data.GeoIP2 (geoContinentCode)
import           Data.GeoIP2 (geoCountry)
import           Data.GeoIP2 (geoCountryISO)
import           Data.GeoIP2 (geoLocation)
import           Data.GeoIP2 (geoPostalCode)
import           Data.GeoIP2 (geoSubdivisions)
import           Data.GeoIP2 (locationAccuracy)
import           Data.GeoIP2 (locationLatitude)
import           Data.GeoIP2 (locationLatitude)
import           Data.GeoIP2 (locationLongitude)
import           Data.GeoIP2 (locationLongitude)
import           Data.GeoIP2 (locationTimezone)
import           Data.GeoIP2 (openGeoDB)
import           Data.IP (IP(..))
import           Data.Text (Text)
import           Geode (app)
import           Network.HTTP.Types.Header (HeaderName)
import           Network.HTTP.Types.Header (hContentType)
import           Test.Hspec (Spec)
import           Test.Hspec (describe)
import           Test.Hspec (hspec)
import           Test.Hspec (it)
import           Test.Hspec.Wai ((<:>))
import           Test.Hspec.Wai (ResponseMatcher(..))
import           Test.Hspec.Wai (get)
import           Test.Hspec.Wai (request)
import           Test.Hspec.Wai (shouldRespondWith)
import           Test.Hspec.Wai (with)
import           Test.Hspec.Wai.JSON (json)

main :: IO ()
main = hspec spec

lookupGeoData :: IP -> Either String GeoResult
lookupGeoData ip
  | ip == (read "34.213.34.148" :: IP) =
    Right $
      GeoResult {
        geoContinent = Just "North America",
        geoContinentCode = Just "NA",
        geoCountryISO = Just "US",
        geoCountry = Just "United States",
        geoLocation = Just $
          Location {
            locationLatitude = 45.8491,
            locationLongitude = -119.7143,
            locationTimezone = "",
            locationAccuracy = Nothing
          },
        geoCity = Just "Boardman",
        geoCityConfidence = Nothing,
        geoPostalCode = Just "97818",
        geoAS = Nothing,
        geoISP = Nothing,
        geoDomain = Nothing,
        geoOrganization = Nothing,
        geoUserType = Nothing,
        geoSubdivisions = [("OR", "Oregon")]
    }
  | otherwise = Left "Information for address does not exist."

expectedJsonResponse :: ResponseMatcher
expectedJsonResponse =
  let ResponseMatcher status _ body = [json|{
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
  }|]
  in  ResponseMatcher status [hContentType <:> "application/json; charset=utf-8"] body

spec :: Spec
spec = with (app lookupGeoData) $ do
  describe "GET /" $ do
    it "responds with 404" $ do
      get "/" `shouldRespondWith` "Information for address does not exist." {matchStatus = 404}

  describe "GET / with X-Forwarded-For header" $ do
    it "responds with geolocation data" $ do
      request "GET" "/" [("X-Forwarded-For" :: HeaderName, "34.213.34.148")] "" `shouldRespondWith` expectedJsonResponse {matchStatus = 200}

  describe "GET /127.0.0.1" $ do
    it "responds with 404" $ do
      get "/127.0.0.1" `shouldRespondWith` "Information for address does not exist." {matchStatus = 404}

  describe "GET /34.213.34.148" $ do
    it "responds with geolocation data" $ do
      get "/34.213.34.148" `shouldRespondWith` expectedJsonResponse {matchStatus = 200}
