{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Bower.Types where

import Data.Aeson
import GHC.Generics
import Data.Text
import Data.Maybe
import Data.Default
import Control.Applicative
import Data.Possible
import Data.Monoid
import qualified Data.Vector as V

import Data.HashMap.Strict (HashMap (..))
import qualified Data.HashMap.Strict as HashMap


data BowerRC =
  BowerRC { directory :: String
          }
    deriving (Show,Generic)

instance FromJSON BowerRC
instance ToJSON BowerRC

-- https://github.com/bower/bower.json-spec


data OneOrList = OneOrList { unOneOrList :: [Text] }

instance FromJSON OneOrList where
--  parseJSON Missing  = pure $  BowerJSON MissingData
  parseJSON (String v) = pure $ OneOrList [v]
  parseJSON (Array a)  = OneOrList . V.toList <$> V.mapM parseJSON a

data BowerJSON =
  BowerJSON { name            :: Text
--            , description     :: Possible Text
--            , version         :: Possible Text
            , main            :: [ Text ]
--            , license         :: Maybe Text or [Text]
--            , ignore          :: Possible [Text]
--            , keywords        :: Possible [Text]
--            , authors         :: Array of (String or Object)
--            , homepage        :: Possible Text
--            , repository      :: Possible Value
            , dependencies    :: (HashMap Text Text)
--            , devDependencies :: Possible Value
--            , resolutions     :: Possible Value
--            , private         :: Possible Bool
            }
    deriving (Show,Generic)

instance Default BowerJSON where
  def = BowerJSON "" [] HashMap.empty

-- instance Monoid BowerJSON
-- instance FromJSON BowerJSON
instance ToJSON   BowerJSON

instance FromJSON BowerJSON where
--  parseJSON Missing  = pure $  BowerJSON MissingData
  parseJSON (Object v) = BowerJSON <$> v .:  "name"
                                   <*> (fmap unOneOrList <$> v.:? "main") .!= []
                                   <*> v .:? "dependencies" .!= HashMap.empty


newtype HideDefault a = HideDefault { unHide :: a }
instance (Default a, Eq a, ToJSON a) => ToJSON (HideDefault a) where
       toJSON a
          | unHide a == def   = Missing
          | otherwise = toJSON a

instance (Default a, Eq a, FromJSON a) => FromJSON (HideDefault a) where
       parseJSON a = HideDefault . fromMaybe def <$> parseJSON a
