{-# LANGUAGE DeriveGeneric #-}
module Types where
import GHC.Generics
import Data.Aeson
import Data.Time (Day)

data Movie = Movie
  {
    mname :: String
    ,rating :: Double
    ,genre :: String
  } deriving (Generic,Show)
instance ToJSON Movie
instance FromJSON Movie

data User = User
  {
    name :: String
    ,age :: Int
    ,email :: String
    ,registration_date :: Day
    ,password :: String
    , favouriteMovie :: [String]
  } deriving (Generic,Show)
instance ToJSON User
instance FromJSON User

