{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Upload
    ( postUploadR
    ) where

import Foundation
import System.Directory
import System.Random.Mersenne
import qualified Data.Map as Map
import Data.Word (Word)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as L

postUploadR :: Handler RepHtml
postUploadR = do
    cknum <- runInputGet $ ireq textField "CKEditorFuncNum"
    upload <- lookupFile "upload" >>= maybe (invalidArgs ["No file found"]) return
    ext <- maybe (invalidArgs ["Invalid file type"]) return
         $ Map.lookup (fileContentType upload) $ Map.fromList
            [ ("image/png", "png")
            , ("image/jpeg", "jpeg")
            , ("image/gif", "gif")
            ]
    num <- liftIO randomIO
    let name = show (num :: Word) ++ '.' : ext
    liftIO $ createDirectoryIfMissing True "static/uploaded"
    liftIO $ L.writeFile ("static/uploaded/" ++ name) $ fileContent upload
    let route = StaticR $ StaticRoute ["uploaded", T.pack name] []
    hamletToRepHtml [hamlet|
<script>
    window.parent.CKEDITOR.tools.callFunction(#{cknum}, "@{route}");
|]
