{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.HTTP.Types.Status (status200, status400, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (Method, methodGet, methodPost, methodOptions)
import Data.Aeson (ToJSON(..), FromJSON(..), Object, Value(..), object, (.=), (.:), decode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Char (toLower)

import SymptomChecker (findMatchingConditions, allSymptoms)

-- Data types for JSON handling
data SymptomRequest = SymptomRequest [Text]
    deriving Show

data SymptomResponse = SymptomResponse [(Text, Int)]
    deriving Show

-- JSON instances
instance FromJSON SymptomRequest where
    parseJSON (Object v) = SymptomRequest <$> v .: "symptoms"
    parseJSON _ = fail "Expected an object"

instance ToJSON SymptomResponse where
    toJSON (SymptomResponse conditions) = object ["conditions" .= conditions]

-- Helper function to convert String to Text
toText :: String -> Text
toText = T.pack

-- Helper function to convert Text to String
fromText :: Text -> String
fromText = T.unpack

-- Main application
app :: Application
app request respond = 
    case (requestMethod request, pathInfo request) of
        ("POST", ["api", "check"]) -> do
            body <- strictRequestBody request
            case decode body of
                Just (SymptomRequest symptoms) -> do
                    let results = findMatchingConditions symptoms
                    respond $ responseLBS status200 
                        [(hContentType, "application/json")] 
                        $ encode $ SymptomResponse results
                Nothing -> respond $ responseLBS status400 
                    [(hContentType, "application/json")] 
                    $ encode $ object ["error" .= ("Invalid request" :: Text)]
        
        ("GET", ["api", "symptoms"]) -> 
            respond $ responseLBS status200 
                [(hContentType, "application/json")] 
                $ encode $ object ["symptoms" .= allSymptoms]
        
        _ -> respond $ responseLBS status404 
            [(hContentType, "application/json")] 
            $ encode $ object ["error" .= ("Not found" :: Text)]

-- CORS middleware configuration
corsMiddleware :: Middleware
corsMiddleware = cors $ const $ Just CorsResourcePolicy
    { corsOrigins = Nothing
    , corsMethods = [methodGet, methodPost, methodOptions]
    , corsRequestHeaders = ["Content-Type"]
    , corsExposedHeaders = Nothing
    , corsMaxAge = Nothing
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }

main :: IO ()
main = do
    putStrLn "Starting server on port 3000..."
    run 3000 $ corsMiddleware app 