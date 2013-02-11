{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Muttonchops where

import Data.ByteString (ByteString)
import Data.List
import Data.Map (Map)
import Data.String.Combinators ((<>))
import Data.Text (Text)
import Snap
import Snap.Snaplet
import System.Directory
import System.FilePath
import Text.Templating.Muttonchops
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import qualified Text.Templating.Muttonchops as M

desc :: Text
desc = "Mustache Templating Engine Snaplet as bindings to Crustache"

data Muttonchops = Muttonchops {
    rawTemplates :: Map FilePath Text
}

class HasMuttonchops app where
    getMuttonchops :: app -> Muttonchops

muttonchopsInit :: FilePath -> SnapletInit app Muttonchops
muttonchopsInit path = makeSnaplet "muttonchops" desc Nothing $ do
    snapletPath <- getSnapletFilePath
    files <- liftIO $ filter (not . ("." `isPrefixOf`)) <$>
        getDirectoryContents (snapletPath </> path)
    withTemplates <- liftIO $ forM files $ \p -> do
        let fullpath = snapletPath </> path </> p
        f <- T.readFile fullpath
        -- see https://github.com/ryb/muttonchops/issues/1
        return (p, f <> "{{workaroundhack}}")
    return $ Muttonchops $ Map.fromList withTemplates

renderAs :: (HasMuttonchops b, HasMuttonchops v) => ByteString -> FilePath
         -> [(Text, Text)] -> Handler b v ()
renderAs mimeType p bindings = do
    Muttonchops rawMap <- gets getMuttonchops
    case Map.lookup p rawMap of
        Nothing -> fail "template not found"
        Just rawTemplate -> do
             modifyResponse (setContentType mimeType)
             writeText $ M.render rawTemplate bindings
