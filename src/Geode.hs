{-# LANGUAGE OverloadedStrings #-}
module Geode (runApp, app) where

import           Data.Aeson ((.=))
import           Data.Aeson (ToJSON)
import           Data.Aeson (object)
import           Data.Aeson (toJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.GeoIP2 (GeoDB)
import           Data.GeoIP2 (GeoResult)
import           Data.GeoIP2 (findGeoData)
import           Data.GeoIP2 (geoCity)
import           Data.GeoIP2 (geoContinent)
import           Data.GeoIP2 (geoContinentCode)
import           Data.GeoIP2 (geoCountry)
import           Data.GeoIP2 (geoCountryISO)
import           Data.GeoIP2 (geoLocation)
import           Data.GeoIP2 (geoPostalCode)
import           Data.GeoIP2 (geoSubdivisions)
import           Data.GeoIP2 (locationLatitude)
import           Data.GeoIP2 (locationLongitude)
import           Data.GeoIP2 (openGeoDB)
import           Data.IP (IP(..))
import           Data.IP (fromHostAddress)
import           Data.Maybe (listToMaybe)
import           Network.HTTP.Types (status404)
import           Network.Socket (SockAddr (SockAddrInet))
import           Network.Wai (Application)
import           Network.Wai (remoteHost)
import           Network.Wai.Middleware.Cors (simpleCors)
import           System.Environment (getEnv)
import           Text.Read (readMaybe)
import           Web.Scotty (ActionM)
import           Web.Scotty (ScottyM)
import           Web.Scotty (file)
import           Web.Scotty (get)
import           Web.Scotty (header)
import           Web.Scotty (middleware)
import           Web.Scotty (param)
import           Web.Scotty (raw)
import           Web.Scotty (request)
import           Web.Scotty (scotty)
import           Web.Scotty (scottyApp)
import           Web.Scotty (setHeader)
import           Web.Scotty (status)
import           Web.Scotty (text)
import qualified Data.Text.Lazy as TL

newtype Result = Result GeoResult

instance ToJSON Result where
  toJSON (Result x) =
    object [ "city"          .= geoCity x
           , "continentCode" .= geoContinentCode x
           , "continent"     .= geoContinent x
           , "countryCode"   .= geoCountryISO x
           , "countryName"   .= geoCountry x
           , "latitude"      .= (locationLatitude <$> geoLocation x)
           , "longitude"     .= (locationLongitude <$> geoLocation x)
           , "postalCode"    .= geoPostalCode x
           , "region"        .= (fst <$> listToMaybe (geoSubdivisions x))
           , "regionName"    .= (snd <$> listToMaybe (geoSubdivisions x))
           ]

type LookupGeoData = IP -> Either String GeoResult

findIp :: ActionM (Maybe IP)
findIp = do
  forM <- header "X-Forwarded-For"
  case forM of
    Just ipTL -> return $ readMaybe $ TL.unpack ipTL
    Nothing -> do
      req <- request
      let ipA = remoteHost req
      case ipA of
        SockAddrInet _ host -> return $ Just (IPv4 $ fromHostAddress host)
        _ -> return Nothing

nope :: String -> ActionM ()
nope e = do
  status status404
  text $ TL.pack e

-- Pretty version of `Web.Scotty (json)`, which calls `ToJSON (encode)`
jsonPretty :: (ToJSON a) => a -> ActionM ()
jsonPretty x = do
  setHeader "Content-Type" "application/json; charset=utf-8"
  raw $ encodePretty x

service :: LookupGeoData -> Maybe IP -> ActionM ()
service _ Nothing = nope "Couldn't figure out your IP address."
service lgd (Just ip) = do
  let geoResult = lgd ip
  case geoResult of
    Left e -> nope e
    Right x -> jsonPretty (Result x)

app' :: LookupGeoData -> ScottyM ()
app' lgd = do
    middleware simpleCors
    get "/" $ do
      ipM <- findIp
      service lgd ipM
    get "/swagger.json" $ do
      setHeader "Content-Type" "application/json"
      file "swagger.json"
    get "/:ip" $ do
      ip <- param "ip"
      service lgd (readMaybe ip)

geodb :: IO GeoDB
geodb = do
  dbname <- getEnv "GEOIP_DB"
  openGeoDB dbname

app :: LookupGeoData -> IO Application
app lgd = do
  scottyApp $ app' lgd

runApp :: IO ()
runApp = do
  port <- read <$> getEnv "PORT"
  gdb <- geodb
  let lgd = findGeoData gdb "en"
  scotty port $ app' lgd
