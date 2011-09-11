{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module FormatHandler where

import qualified Data.Set as Set
import qualified Data.Text as T
import Yesod.Form
import Yesod.Form.Jquery
import Yesod.Core
import Text.Blaze (Html, preEscapedText, toHtml)
import Text.Lucius (lucius)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import Text.Hamlet (shamlet)
import Data.Maybe (listToMaybe)
import Text.HTML.SanitizeXSS (sanitizeBalance)
import Text.Julius (julius)
import Data.Enumerator (($$), run_)
import Data.Enumerator.List (consume)
import qualified Data.ByteString as S
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.URI.Enumerator
import Network.URI.Enumerator.File
import Control.Monad (liftM, unless)
import DITA.Parse (loadTopicTrees, runDITA, topicTreesToDoc)
import DITA.Output.HTML
import DITA.Util.Render
import DITA.Util.Naming
import DITA.Util.ClassMap (ClassMap)
import qualified Data.Map as Map
import Text.XML
import Text.XML.Xml2Html ()
import qualified Data.Text.Lazy as TL
import Control.Monad.Trans.State (evalState, get, put)

data FormatHandler master = FormatHandler
    { fhExts :: Set.Set Ext
    , fhName :: T.Text
    , fhForm :: forall sub. RenderMessage master FormMessage => Maybe T.Text -> Html -> Form sub master (FormResult T.Text, GWidget sub master ())
    , fhWidget :: forall sub. SchemeMap IO -> URI -> GWidget sub master ()
    }

type Ext = T.Text

textFormatHandler :: FormatHandler master
textFormatHandler = FormatHandler
    { fhExts = Set.singleton "txt"
    , fhName = "Plain text"
    , fhForm = (fmap . fmap) (\(a, b) -> (fmap unTextarea a, b >> toWidget css))
             . renderTable
             . areq textareaField "Content"
             . fmap Textarea
    , fhWidget = \sm uri -> do
        id' <- lift newIdent
        t <- liftIO $ uriToText sm uri
        toWidget [lucius|##{id'} { white-space: pre }|]
        toWidget [shamlet|<div ##{id'}>#{t}|]
    }
  where
    css = [lucius|textarea { width: 500px; height: 400px } |]

uriToText :: Monad m => SchemeMap m -> URI -> m T.Text
uriToText sm uri = liftM (decodeUtf8With lenientDecode . S.concat) $ run_ $ readURI sm uri $$ consume

htmlFormatHandler :: (YesodAloha master, YesodJquery master) => FormatHandler master
htmlFormatHandler = FormatHandler
    { fhExts = Set.singleton "html"
    , fhName = "HTML"
    , fhForm = renderTable . areq alohaHtmlField "Content"
    , fhWidget = \sm uri -> do
        t <- liftIO $ uriToText sm uri
        toWidget $ preEscapedText t
    }

class YesodAloha a where
    urlAloha :: a -> Either (Route a) T.Text
    urlAlohaPlugins :: a -> [Either (Route a) T.Text]

alohaHtmlField :: (YesodAloha master, YesodJquery master) => Field sub master T.Text
alohaHtmlField = Field
    { fieldParse = return . Right . fmap sanitizeBalance . listToMaybe
    , fieldView = \theId name val _isReq -> do
        y <- lift getYesod
        addScriptEither $ urlJqueryJs y
        addScriptEither $ urlAloha y
        mapM_ addScriptEither $ urlAlohaPlugins y
        toWidget [shamlet|
<div ##{theId}-container>
    <textarea ##{theId} name=#{name}>#{showVal val}
|]
        toWidget [julius|$(function(){$("##{theId}").aloha();})|]
        toWidget [lucius|##{theId}-container { width: 800px; height: 400px; }|]
    }
  where
    showVal = either id id

findHandler :: Ext -> [FormatHandler m] -> Maybe (FormatHandler m)
findHandler _ [] = Nothing
findHandler e (fh:fhs)
    | e `Set.member` fhExts fh = Just fh
    | otherwise = findHandler e fhs

ditaFormatHandler :: URI -- ^ catalog
                  -> ClassMap
                  -> FormatHandler master
ditaFormatHandler catalog classmap = FormatHandler
    { fhExts = Set.fromList ["xml", "dita"]
    , fhName = "DITA Topic"
    , fhForm = (fmap . fmap) (\(a, b) -> (fmap unTextarea a, b >> toWidget css))
             . renderTable
             . areq (check (fmap Textarea . validXML . unTextarea) textareaField) "Content"
             . fmap Textarea
    , fhWidget = \sm uri -> do
        let sm' = Map.insert "file:" fileScheme sm
        ex <- liftIO $ runDITA catalog "dtd-flatten.jar" sm' $ do
            -- FIXME we want to cache the results here somehow
            tts <- loadTopicTrees uri
            doc <- topicTreesToDoc uri tts
            let ro = docToHtmlDocs def { hsClassMap = classmap } doc
            let mdoc = fmap snd $ listToMaybe $ filter (\(x, _) -> x /= RelPath "index.html") $ Map.toList $ roDocs ro
            let nodes = maybe [] (childrenOf "article" . documentRoot) mdoc
            return (maybe T.empty (T.concat . concatMap text . childrenOf "title" . documentRoot) mdoc, nodes)
        case ex of
            Left{} -> toWidget [shamlet|<p>Invalid DITA content|]
            Right (title, nodes) -> do
                unless (T.null title) $ setTitle $ toHtml title
                toWidget $ mapM_ toHtml nodes
    }
  where
    css = [lucius|textarea { width: 500px; height: 400px } |]
    childrenOf name (Element n _ ns)
        | name == n = ns
        | otherwise = concatMap (go name) ns
    go name (NodeElement e) = childrenOf name e
    go _ _ = []
    text (NodeElement (Element _ _ ns)) = concatMap text ns
    text (NodeContent t) = [t]
    text _ = []
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
