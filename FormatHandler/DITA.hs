{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module FormatHandler.DITA
    ( ditaFormatHandler
    ) where

import qualified Data.Text as T
import FormatHandler
import Text.Lucius (lucius)
import DITA.Parse (loadTopicTrees, runDITA)
import DITA.Output.HTML (renderTopicTree, hsClassMap)
import DITA.Util.Render
import DITA.Util.ClassMap (ClassMap)
import qualified Data.Map as Map
import Text.XML
import Text.XML.Xml2Html ()
import qualified Data.Text.Lazy as TL
import Control.Monad.Trans.State (evalState, get, put)
import qualified Text.XML.Catalog as C
import DITA.Types (topicTitle, ttTopic, Href)
import DITA.Util (text)
import Control.Monad (unless)
import Network.URI.Enumerator.File
import Yesod.Core
import Yesod.Form
import qualified Data.Set as Set
import Text.Hamlet (shamlet)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
import Text.Blaze (toHtml)

ditaFormatHandler :: (Href -> T.Text)
                  -> C.DTDCache IO
                  -> ClassMap
                  -> FormatHandler master
ditaFormatHandler renderHref' cache classmap = FormatHandler
    { fhExts = Set.fromList ["xml", "dita"]
    , fhName = "DITA Topic"
    , fhForm = (fmap . fmap) (\(a, b) -> (fmap unTextarea a, b >> toWidget css))
             . renderTable
             . areq (check (fmap Textarea . validXML . unTextarea) textareaField) "Content"
             . fmap Textarea
    , fhWidget = \sm uri -> do
        let sm' = Map.insert "file:" fileScheme sm
        ex <- liftIO $ runDITA cache sm' $ do
            -- FIXME we want to cache the results here somehow
            tts <- loadTopicTrees uri
            let ri topic = RenderInfo
                    { riTopic = topic
                    , riMisc = def { hsClassMap = classmap }
                    , riParent = Nothing
                    , riChildren = []
                    , riRelTable = []
                    , riGetLinkText = const "<Link text not enabled yet>"
                    , riRenderNav = const Nothing
                    , riRenderHref = renderHref'
                    }
            let nodes = concatMap (renderTopicTree ri) tts
            let title = maybe "" (text . topicTitle . ttTopic)
                      $ listToMaybe tts
            return (title, nodes)
        case ex of
            Left{} -> toWidget [shamlet|<p>Invalid DITA content|]
            Right (title, nodes) -> do
                unless (T.null title) $ setTitle $ toHtml title
                toWidget $ mapM_ toHtml nodes
    }
  where
    css = [lucius|textarea { width: 500px; height: 400px } |]
    validXML t =
        case parseText def $ TL.fromChunks [t] of
            Left{} -> Left ("Invalid XML" :: T.Text)
            Right (Document a root b) -> Right $ TL.toStrict $ renderText def $ Document a (fixIds root) b
    fixIds root = flip evalState (Set.empty, 1 :: Int) $ do
        root' <- checkUnused root
        addIds root'
    checkUnused (Element e as ns) = do
        as' <-
            case lookup "id" as of
                Nothing -> return as
                Just id' -> do
                    (used, i) <- get
                    if id' `Set.member` used
                        then return $ filter (\(x, _) -> x /= "id") as
                        else put (Set.insert id' used, i) >> return as
        ns' <- mapM checkUnused' ns
        return $ Element e as' ns'
    checkUnused' (NodeElement e) = fmap NodeElement $ checkUnused e
    checkUnused' n = return n
    addIds (Element e as ns) = do
        as' <-
            case lookup "id" as of
                Just{} -> return as
                Nothing -> do
                    i <- nextId
                    return $ ("id", i) : as
        ns' <- mapM addIds' ns
        return $ Element e as' ns'
    addIds' (NodeElement e) = fmap NodeElement $ addIds e
    addIds' n = return n
    nextId = do
        (used, i) <- get
        let (id', i') = go' used i
        put (Set.insert id' used, i')
        return id'
      where
        go' used i =
            let id' = "x-" `T.append` T.pack (show i)
             in if id' `Set.member` used
                    then go' used $ i + 1
                    else (id', i)
