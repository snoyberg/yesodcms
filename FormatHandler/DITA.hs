{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module FormatHandler.DITA
    ( ditaFormatHandler
    , ditamapFormatHandler
    ) where

import qualified Data.Text as T
import FormatHandler
import Text.Lucius (lucius)
import DITA.Parse (loadTopicTrees, runDITA, loadDoc)
import DITA.Output.HTML (renderTopicTree, hsClassMap, hsGoElem, HtmlSettings)
import DITA.Util.Render
import DITA.Util.ClassMap (ClassMap)
import Text.XML
import Text.XML.Xml2Html ()
import qualified Data.Text.Lazy as TL
import Control.Monad.Trans.State (evalState, get, put)
import qualified Text.XML.Catalog as C
import DITA.Types (topicTitle, ttTopic, Href, Doc (..), Nav (..), NavId (..), Class (..), ttChildren, topicContent)
import DITA.Util (text)
import Control.Monad (unless)
import Yesod.Core
import Yesod.Form
import qualified Data.Set as Set
import Text.Hamlet (shamlet)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (listToMaybe, maybeToList)
import Text.Blaze (toHtml, Html)
import Text.Hamlet.XML (xml)
import qualified Data.Map as Map
import Data.Enumerator (Enumerator)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified DITA.Types as D
import Data.IORef
import Network.URI.Enumerator (URI)
import Data.Time
import qualified Network.HTTP.Types as H
import Blaze.ByteString.Builder.Char.Utf8 (fromChar, fromText)
import Data.Monoid (mconcat)
import Data.Text.Encoding (decodeUtf8)
import Blaze.ByteString.Builder (toByteString, fromByteString)
import Network.Wai (rawQueryString, rawPathInfo)
import Network.HTTP.Types (parseQuery)

ditaFormatHandler :: (Href -> T.Text)
                  -> C.DTDCache IO
                  -> ClassMap
                  -> (URI -> IO D.FileId)
                  -> FormatHandler master
ditaFormatHandler renderHref' cache classmap loadFileId = FormatHandler
    { fhExts = Set.fromList ["xml", "dita"]
    , fhName = "DITA Topic"
    , fhForm = xmlForm "Content"
    , fhWidget = widget
    , fhFlatWidget = widget
    , fhFilter = xmlFilter
    , fhRefersTo = const $ const $ return []
    , fhTitle = \sm uri -> fmap (either (const Nothing) id) $ runDITA cache sm (Just loadFileId) $ do
        tts <- loadTopicTrees uri
        return $ fmap (text . topicTitle . ttTopic) $ listToMaybe tts
    , fhToText = \sm uri -> fmap (either (const Nothing) (Just . plain)) $ runDITA cache sm (Just loadFileId) $ loadTopicTrees uri
    }
  where
    -- Convert a list of topic trees to plain text, used for the search index
    plain :: [D.TopicTree] -> T.Text
    plain = T.concat . concatMap plainTT

    plainTT :: D.TopicTree -> [T.Text]
    plainTT tt = concat $ plainT (ttTopic tt) : map plainTT (ttChildren tt)

    plainT :: D.Topic -> [T.Text]
    plainT topic = concatMap plainE $ topicTitle topic : topicContent topic

    plainE :: D.Element -> [T.Text]
    plainE (D.Element _ _ _ ns) = concatMap plainN ns

    plainN :: D.Node -> [T.Text]
    plainN (D.NodeContent t) = [t]
    plainN (D.NodeElement e) = plainE e
    plainN _ = []

    widget sm uri = do
        ex <- liftIO $ runDITA cache sm (Just loadFileId) $ do
            tts <- loadTopicTrees uri
            let ri topic = RenderInfo
                    { riTopic = topic
                    , riMisc = def
                        { hsClassMap = classmap
                        , hsGoElem = goElem
                        }
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
            Left e -> toWidget [shamlet|<p>Invalid DITA content: #{show e}|]
            Right (title, nodes) -> do
                unless (T.null title) $ setTitle $ toHtml title
                toWidget $ mapM_ toHtml nodes

ditamapFormatHandler :: (RenderMessage master FormMessage, Show (Route master))
                     => (Href -> T.Text)
                     -> C.DTDCache IO
                     -> ClassMap
                     -> (URI -> IO D.FileId)
                     -> IORef (Map.Map URI (UTCTime, Doc))
                     -> (URI -> NavId -> D.FileId -> D.TopicId -> (Route master, [(T.Text, T.Text)]))
                     -> FormatHandler master
ditamapFormatHandler renderHref' cache classmap loadFileId idocCache toNavRoute = FormatHandler
    { fhExts = Set.fromList ["ditamap"]
    , fhName = "DITA Map"
    , fhForm = xmlForm "Content"
    , fhWidget = \sm uri -> do
        mtopic <- lift $ runInputGet $ iopt textField "topic"
        case mtopic of
            Nothing -> return ()
            Just topic -> do
                -- Due to the redirect middleware messing around with stuff, we
                -- can't trust the actual queryString (and therefore, the
                -- reqGetParams). Instead, we need to look at the
                -- rawQueryString instead.
                gets <- fmap (filter (\(x, _) -> x /= "topic") . parseQuery . rawQueryString) $ lift waiRequest
                rpi <- fmap rawPathInfo $ lift waiRequest
                lift $ redirectText RedirectPermanent $ decodeUtf8 $ toByteString $ mconcat
                    [ fromByteString rpi
                    , H.renderQueryBuilder True gets
                    , fromChar '#'
                    , fromText topic
                    ]
        mnavid <- lift $ runInputGet $ iopt textField "nav"

        r <- lift getUrlRender
        mcr <- lift getCurrentRoute
        tm <- lift getRouteToMaster
        let root = maybe "" (r . tm) mcr

        ex <- liftIO $ runDITA cache sm (Just loadFileId) $ do
            doc <- cacheLoad uri

            return $ case mnavid >>= flip Map.lookup (docNavMap doc) . NavId of
                Nothing -> (docTitle doc, wrapper Nothing (showNavs root (docNavs doc)) [])
                Just nav -> (navTitle nav, wrapper (Just $ navTitle nav) (showNavs root (docNavs doc)) (showNav makeRi nav))
        case ex of
            Left e -> toWidget [shamlet|<p>Invalid DITA map: #{show e}|]
            Right (title, nodes) -> do
                unless (T.null title) $ setTitle $ toHtml title
                toWidget $ mapM_ toHtml nodes
    , fhFilter = xmlFilter
    , fhRefersTo = \sm uri -> do
        edoc <- runDITA cache sm (Just loadFileId) $ loadDoc uri
        case edoc of
            Left{} -> return []
            Right doc -> do
                let navPairs = concatMap deepPairs $ docNavs doc
                let go nav topic = (D.topicSource topic, toNavRoute uri nav (D.topicFileId topic) (D.topicId topic))
                return $ concatMap (\(nav, tt) -> map (go nav) $ deepTopics tt) navPairs
    , fhTitle = \sm uri -> fmap (either (const Nothing) Just) $ runDITA cache sm (Just loadFileId) $ fmap docTitle $ cacheLoad uri
    , fhFlatWidget = \sm uri -> do
        ex <- liftIO $ runDITA cache sm (Just loadFileId) $ cacheLoad uri
        case ex of
            Left e -> toWidget [shamlet|<p>Error parsing DITA map: #{show e}|]
            Right doc -> toWidget $ mapM_ toHtml $ concatMap (showNavsDeep makeRi) $ docNavs doc
    , fhToText = const $ const $ return Nothing
    }
  where
    makeRi topic = RenderInfo
        { riTopic = topic
        , riMisc = def
            { hsClassMap = classmap
            , hsGoElem = goElem
            }
        , riParent = Nothing
        , riChildren = []
        , riRelTable = []
        , riGetLinkText = const "<Link text not enabled yet>"
        , riRenderNav = const Nothing
        , riRenderHref = renderHref'
        }
    cacheLoad uri = do
        docCache <- liftIO $ readIORef idocCache
        mdoc <-
            case Map.lookup uri docCache of
                Nothing -> return Nothing
                Just (toUpdate, doc) -> do
                    now <- liftIO getCurrentTime
                    return $ if now > toUpdate then Nothing else Just doc
        case mdoc of
            Nothing -> do
                doc <- loadDoc uri
                now <- liftIO getCurrentTime
                let toUpdate = addUTCTime (60 * 5) now
                liftIO $ atomicModifyIORef idocCache (\m -> (Map.insert uri (toUpdate, doc) m, ()))
                return doc
            Just doc -> return doc

    deepPairs nav = maybeToList (navTopicTree nav) ++ concatMap deepPairs (navChildren nav)

    deepTopics tt = ttTopic tt : concatMap deepTopics (ttChildren tt)

    wrapper mtitle toc content = [xml|
<nav id=maptoc>
    ^{toc}
<article id=mapcontent>
    $maybe title <- mtitle
        <h1>#{title}
    ^{content}
|]
    showNavs _ [] = []
    showNavs root navs = [xml|
<ul>
    $forall nav <- navs
        <li>
            $maybe ntt <- navTopicTree nav
                <a href="#{root}?nav=#{unNavId $ fst ntt}">
                    \#{navTitle nav}
            $nothing
                \#{navTitle nav}
            ^{showNavs root $ navChildren nav}
|]
    showNav ri Nav { navTopicTree = Just (_, tt) } = renderTopicTree ri tt
    showNav _ _ = []

    showNavsDeep ri nav = [xml|
<h1>#{navTitle nav}
$maybe tt <- navTopicTree nav
    ^{renderTopicTree ri $ snd tt}
$forall nav <- navChildren nav
    <section .subtopic>
        ^{showNavsDeep ri nav}
|]

xmlForm :: (RenderMessage master FormMessage, RenderMessage master msg)
        => FieldSettings msg -> Maybe T.Text -> Html -> Form sub master (FormResult T.Text, GWidget sub master ())
xmlForm fs =
      (fmap . fmap) (\(a, b) -> (fmap unTextarea a, b >> toWidget css))
    . renderTable
    . areq (check (fmap Textarea . validXML . unTextarea) textareaField) fs
    . fmap Textarea
  where
    css = [lucius|textarea { width: 500px; height: 400px } |]


validXML :: T.Text -> Either T.Text T.Text
validXML t =
    case parseText def $ TL.fromChunks [t] of
        Left{} -> Left "Invalid XML"
        Right (Document a root b) -> Right $ TL.toStrict $ renderText def $ Document a (fixIds root) b

fixIds :: Element -> Element
fixIds root = flip evalState (Set.empty, 1 :: Int) $ do
    root' <- checkUnused root
    addIds root'
  where
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

xmlFilter :: L.ByteString -> Maybe (Enumerator ByteString IO a)
xmlFilter lbs =
    case parseLBS def lbs of
        Left{} -> Nothing
        Right (Document a root b) -> Just $ renderBytes def $ Document a (fixIds root) b

goElem :: Class -> RenderInfo HtmlSettings -> D.Element -> Maybe [Node]
goElem _ _ _ = Nothing
