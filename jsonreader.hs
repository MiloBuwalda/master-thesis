{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- "H:\Documents\\Dropbox\\Study\\E-Learning\\Small project\\data"
-- :set -XOverloadedStrings
-- :set -XRecordWildCards
module JsonReader where

-- import Prelude hiding (id)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
-- import Data.Aeson.Types -- PARSERS
-- import Data.Traversable (traverse)
-- import Data.Foldable (toList)
-- import Control.Applicative
-- import qualified Data.Text as Txt

laptopDropboxPath, pcDropboxPath, astsHOC4, astsHOC18 :: String
laptopDropboxPath = "C:\\Users\\Milo\\"
pcDropboxPath = "H:\\Documents\\"
-- astsHOC4 =  pcDropboxPath++"Dropbox\\Study\\E-Learning\\Small project\\data\\hoc4\\asts"
-- astsHOC18 = pcDropboxPath++"Dropbox\\Study\\E-Learning\\Small project\\data\\hoc18\\asts"

astsHOC4 =  pcDropboxPath++"Dropbox\\Study\\E-Learning\\Master Thesis\\data\\hoc4\\asts"
astsHOC18 = pcDropboxPath++"Dropbox\\Study\\E-Learning\\Master Thesis\\data\\hoc18"

-- data CValue = Children [CValue] 
--             | TypeM Txt.Text 
--             | ID Txt.Text
--             deriving Show

-- instance FromJSON CValue where
--   parseJSON v = 
--         withText "type" (pure . TypeM) v
--     <|> withText "id" (pure . ID) v
--     <|> withArray "children" (\a -> Children . toList <$> traverse parseJSON a) v


data AST = AST {
    astID :: String
  , children  :: [AST]
  , typeM :: String 
  } deriving Show

  -- deriving (Generic)

-- instance FromJSON AST where
--     parseJSON (Object v) = AST <$> 
--                            v .: "id" <*> 
--                            v .:? "children" <*> 
--                            v .: "type"
--     parseJSON _ = mempty

instance FromJSON AST where
    parseJSON = withObject "ast" $ \o -> do
        astID <- o .: "id"
        typeM <- o .: "type"
        children <- o .:? "children" .!= [] --parseJSON (Object o)
        return AST{..}


jsonFile :: Int -> FilePath
jsonFile astID = astsHOC18 ++ "\\asts\\" ++ show astID ++ ".json"

unseenFile :: String -> FilePath
unseenFile astID = astsHOC18 ++ "\\unseen\\" ++ astID ++ ".json"

getJSON :: Int -> IO B.ByteString
getJSON astID = B.readFile $ jsonFile astID 

getUnseenJSON :: String -> IO B.ByteString
getUnseenJSON astID = B.readFile $ unseenFile astID 

-- readJSON :: IO ( Either String AST)
readSeen :: Int -> IO (Maybe AST)
readSeen astID = (decode <$> getJSON astID) :: IO (Maybe AST)
    -- if d == Left -> Error
    -- otherwise -> WORKS 
    -- case d of
    --     Left err -> putStrLn (err ++ " whut")
    --     Right ps -> print ps --return $ ps 


readUnseen :: String -> IO (Maybe AST)
readUnseen astID = (decode <$> getUnseenJSON astID) :: IO (Maybe AST)

readJSON :: String -> IO (Maybe AST)
readJSON astID = if isUnseen astID then readUnseen astID else readSeen (read astID::Int)


isUnseen :: String -> Bool
isUnseen s = head s == 'i'



-- readJSON :: IO (Either String AST)
-- readJSON = do
--             ast <- (eitherDecode <$> getJSON) :: IO (Either String AST)
--             case ast of
--                 Left err -> Left putStrLn (err ++ "Error decoding.")
--                 Right result -> _
