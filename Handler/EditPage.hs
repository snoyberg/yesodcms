{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.EditPage
    ( getEditPageR
    , postEditPageR
    ) where

import Foundation
import qualified Data.Text as T
import FormatHandler
import FileStore
import Data.Enumerator (($$), run_, enumList)
import Data.Enumerator.List (consume)
import qualified Data.ByteString as S
import Data.Maybe (isJust)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

getEditPageR :: [T.Text] -> Handler RepHtml
getEditPageR ts = do
    -- FIXME validate that there are no invalid paths (leading dots, slashes), check permissions
    let ext = snd $ T.breakOnEnd "." $ safeLast "" ts
    Cms { formatHandlers = fhs, fileStore = fs } <- getYesod
    fh <- maybe (invalidArgs ["Invalid file extension: " `T.append` ext]) return $ findHandler ext fhs
    let t = T.intercalate "/" ts
    mecontents <- liftIO $ fsGetFile fs t
    mcontents <- liftIO $
        case mecontents of
            Nothing -> return Nothing
            Just econtents -> fmap (Just . decodeUtf8With lenientDecode . S.concat) $ run_ $ econtents $$ consume
    ((res, widget), enctype) <- runFormPost $ fhForm fh mcontents
    case res of
        FormSuccess c -> do
            liftIO $ fsPutFile fs t $ enumList 1 [encodeUtf8 c]
            setMessage "File contents updated"
        _ -> return ()
    let toView = isJust mcontents || isSucc res
    defaultLayout $(widgetFile "edit-page")
  where
    isSucc FormSuccess{} = True
    isSucc _ = False
    safeLast t [] = t
    safeLast _ x = last x

routes :: [T.Text] -> [(T.Text, Route Cms)]
routes ("wiki":rest) = [("Wiki page", WikiR $ safeInit rest)]
routes ("home":uid:rest) = [("Home folder", UserFileIntR uid rest)]
routes ["page", _] = [("Homepage", RootR)]
routes ("page":x:xs) = [("Static page", PageR x $ safeInit xs)]
routes _ = []

safeInit :: [a] -> [a]
safeInit [] = []
safeInit x = init x

postEditPageR :: [T.Text] -> Handler RepHtml
postEditPageR = getEditPageR
