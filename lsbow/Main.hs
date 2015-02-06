{-# Language LambdaCase
           , OverloadedStrings
           , ScopedTypeVariables
           , ViewPatterns
           #-}

module Main where

import System.Directory
import Data.Bool

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T


import Data.Monoid
import Data.Maybe
import qualified Data.List as DL

import qualified Bower.Types as BT

import qualified Data.Aeson as DA
import qualified Data.Aeson.Types as DA
import qualified Data.ByteString.Lazy as B

import Control.Arrow

import Control.Applicative

import qualified Data.HashMap.Strict as HM

-- | default bower config
defFolder :: String
defFolder = "bower_components"

-- | default target folder
defTarget :: Text
defTarget = "static-embeded"

-- | bower config file
defRC :: String
defRC = ".bowerrc"

defYB :: String
defYB = ".ybow"


getWebComponents :: IO [Text]
getWebComponents = doesFileExist defYB >>= bool (return []) (T.words <$> T.readFile defYB)

-- | check .bowerrc if the default folder is still correct
getBaseDir :: IO String
getBaseDir = doesFileExist defRC >>= bool (return defFolder) readRc

-- | extract directory name from bowerrc
readRc = (DA.decode <$> B.readFile defRC) >>= \case
             Nothing -> return defFolder
             Just v  -> return (BT.directory v)

-- | parse bower.json at some location
readBowerJSON :: String ->  IO (Maybe BT.BowerJSON)
readBowerJSON f = DA.decode <$> B.readFile f

-- | get dependencies from it
getDeps :: Maybe BT.BowerJSON -> [Text]
getDeps = maybe [] (HM.keys . BT.dependencies)

-- | get main files 
getMains :: Maybe BT.BowerJSON -> [Text]
getMains = maybe [] BT.main

-- | traverse all referenced modules, and build dependency tree from them
recurseDeps :: Text -> [Text] -> [Text] -> IO [(Text, [Text],[Text])]
recurseDeps _ _ [] = return []
recurseDeps bd vs (a:as) = do
        if  a `elem` vs
          then recurseDeps bd vs as
          else do
             (b1,mf) <- (getDeps &&& getMains) <$> readBowerJSON (T.unpack $ bd <> "/" <>  a <> "/bower.json")
             bs <- recurseDeps bd (a:vs) (as <> b1)
             return ((a,b1,mf) : bs)
-- recurseDeps bd

-- | we should be fine concatenating all JS files, but css and files needs to be relative

deTree :: Text -> [(Text, [Text],[Text])] -> [(Text, Text)]
            -- |     name deps files
-- ^ take base dir and our sorted tree and produce list of relative path, absolute
deTree _ [] = []
deTree bd ((n,_,fs):as) = (DL.map (\(dropDot -> f) -> (f, bd <> "/" <> n <> "/" <> f) )  fs) <> deTree bd as

dropDot :: Text -> Text
-- ^ some files have "./" in the beginning so, drop it
dropDot t = bool t (T.drop 2 t)  ( "./" `T.isPrefixOf` t )

dropDist :: Text -> Text
-- ^ some files have "./" in the beginning so, drop it
dropDist t = bool t (T.drop 5 t)  ( "dist/" `T.isPrefixOf` t )

main = do
--     putStrLn "bow"
     bd   <-  T.pack <$>  getBaseDir
     wc   <- getWebComponents
     tree <- DL.sortBy deps <$> ( recurseDeps bd [] =<< getDeps <$> readBowerJSON "bower.json" )

     let (webc, tree') = DL.partition (\(a,_,_)-> a `elem` wc) tree
--     print tree
         dt = deTree bd tree'
         (jss, rest) = DL.partition  (T.isSuffixOf ".js" . fst) dt
         (css, _)    = DL.partition  (T.isSuffixOf ".css" . fst) rest
       -- ^  split into js and rest
         toMin  = T.unwords . DL.map ( ("../" <> ). snd) $ jss
         refCss = T.unwords . DL.map ( (\a -> "@import url('" <> dropDist a <> "');\\n") . fst) $ css
--     mapM_ print jss
--     mapM_ print rest

     T.putStrLn $ "#!/bin/bash\n\nclu app.min " <> toMin
     T.putStrLn $ T.unlines $ map (\(a,_,_) -> "cp -r ../" <> bd <> "/" <> a <> " .") $ filter (\(_,_,a) -> DL.null a ) tree'
     -- T.putStrLn $ T.unlines $ map (\(a,_,_) -> "cp -r ../" <> bd <> "/" <> a <> " .") $ webc
     -- ^ we will need to run vulcanize over them
     T.putStrLn $ generateRestCP rest
     T.putStrLn $ "echo -e \" " <> refCss <> "\" > app.css"
     T.putStrLn $ "\ncd .."

generateRestCP ls = T.unlines $ DL.map  (\(a,b) -> let dt = ( fst . T.breakOn "*" . dropDist $ a) in "mkdir -p `dirname "<> dt <>"x` && cp -r ../" <> b <> " " <> dt ) ls
 -- maybe replace it by find


-- | sort no dep first, then push back all that need the previous
deps (n1, [] , _) (n2, dl2, _) = LT
deps (n1, dl1 , _) (n2, [], _) = GT
deps (n1, dl1 , _) (n2, dl2, _) = case (n1 `elem` dl2, n2 `elem` dl1) of
         (True,  True ) -> EQ -- trace "circular deps" 
         -- ^ should be error as they need eachother
         (False, False) -> EQ
         (True, False ) -> LT
         (False, True ) -> GT
