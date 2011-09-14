{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Search
    ( getSearchR
    , getSearchXmlpipeR
    ) where

import Foundation
import Data.Maybe (fromMaybe)
import Text.Search.Sphinx
import qualified Text.Search.Sphinx.Types as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.XML as X
import Text.Hamlet.XML (xml)
import FileStore
import FormatHandler
import Data.Maybe (catMaybes)
import Control.Monad (forM)
import Database.Persist.Base
import qualified Text.Search.Sphinx.ExcerptConfiguration as E
import Data.Text.Lazy.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (ignore)
import Text.Blaze (preEscapedLazyText)

data MInfo = MInfo
    { miFile :: T.Text
    , miTitle :: T.Text
    , miExcerpt :: TL.Text
    }

getSearchR :: Handler RepHtml
getSearchR = do
    mquery <- runInputGet $ iopt textField "q"
    mres <- maybe (return Nothing) (fmap Just . liftIO . query config "yesodcms" . T.unpack) mquery
    let results =
            case mres of
                Nothing -> return ()
                Just (S.Ok qr) -> do
                    mis <- lift $ runDB $ fmap catMaybes $ forM (S.matches qr) $ \S.Match { S.documentId = did } -> do
                        let fid = Key $ PersistInt64 did
                        mf <- get fid
                        case mf of
                            Just FileName
                                { fileNameTitle = Just title
                                , fileNameContent = Just content
                                , fileNameUri = uri
                                } -> do
                                    let escape '<' = "&lt;"
                                        escape '>' = "&gt;"
                                        escape '&' = "&amp;"
                                        escape c = T.singleton c
                                    rexcerpt <- liftIO $ buildExcerpts E.altConfig { E.port = 9312 } [T.unpack $ T.concatMap escape content] "yesodcms" $ T.unpack $ fromMaybe "" mquery
                                    case rexcerpt of
                                        S.Ok bss ->
                                            return $ Just MInfo
                                                { miFile = T.drop 1 $ T.dropWhile (/= ':') uri
                                                , miTitle = title
                                                , miExcerpt = TL.concat $ map (decodeUtf8With ignore) bss
                                                }
                                        _ -> return Nothing
                            _ -> return Nothing
                    $(widgetFile "search-results")
                Just x -> [whamlet|<p>Error running search: #{show x}|]
    defaultLayout $(widgetFile "search")
  where
    config = defaultConfig
        { port = 9312
        , mode = S.Any
        }

getSearchXmlpipeR :: Handler RepXml
getSearchXmlpipeR = do
    Cms { formatHandlers = fhs, fileStore = fs } <- getYesod
    files <- runDB $ selectList [] [] -- FIXME it would be nice to use an enum, but we have to be able to write to the DB inside the iter
    docs <- fmap catMaybes $ runDB $ forM files $ \(fid, f) -> do
        let t = T.drop 1 $ T.dropWhile (/= ':') $ fileNameUri f
        case findHandler (snd $ T.breakOnEnd "." t) fhs of
            Nothing -> return Nothing
            Just fh -> do
                muri <- liftIO $ fsGetFile fs t
                case muri of
                    Nothing -> return Nothing
                    Just uri -> do
                        mtext <- liftIO $ fhToText fh (fsSM fs) uri
                        case mtext of
                            Nothing -> return Nothing
                            Just text -> do
                                title <- lift $ fileTitle t
                                update fid [FileNameTitle =. Just title, FileNameContent =. Just text]
                                return $ Just (fid, T.concat [title, " ", text])
    return $ RepXml $ toContent $ X.renderLBS X.def $ flip (X.Document (X.Prologue [] Nothing [])) [] $ X.Element "sphinx:docset" [] [xml|
<sphinx:schema>
    <sphinx:field name=content>
$forall doc <- docs
    <sphinx:document id=#{toSinglePiece $ fst doc}>
        <content>#{snd doc}
|]
