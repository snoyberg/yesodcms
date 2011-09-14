{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
module FormatHandler
    ( FormatHandler (..)
    , uriToText
    , findHandler
    ) where

import qualified Data.Set as Set
import qualified Data.Text as T
import Yesod.Core
import Yesod.Form
import Text.Blaze (Html)
import Data.Enumerator (($$), run_, Enumerator)
import Data.Enumerator.List (consume)
import qualified Data.ByteString as S
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.URI.Enumerator
import Control.Monad (liftM)
import qualified Data.ByteString.Lazy as L

data FormatHandler master = FormatHandler
    { fhExts :: Set.Set Ext
    , fhName :: T.Text
    , fhForm :: forall sub. RenderMessage master FormMessage => Maybe T.Text -> Html -> Form sub master (FormResult T.Text, GWidget sub master ())
    , fhWidget :: forall sub. SchemeMap IO -> URI -> GWidget sub master ()
    , fhFilter :: L.ByteString -> Maybe (Enumerator S.ByteString IO ())
    , fhRefersTo :: SchemeMap IO -> URI -> IO [(URI, (Route master, [(T.Text, T.Text)]))]
    , fhTitle :: SchemeMap IO -> URI -> IO (Maybe T.Text)
    , fhFlatWidget :: forall sub. SchemeMap IO -> URI -> GWidget sub master ()
    }

type Ext = T.Text

uriToText :: Monad m => SchemeMap m -> URI -> m T.Text
uriToText sm uri = liftM (decodeUtf8With lenientDecode . S.concat) $ run_ $ readURI sm uri $$ consume

findHandler :: Ext -> [FormatHandler m] -> Maybe (FormatHandler m)
findHandler _ [] = Nothing
findHandler e (fh:fhs)
    | e `Set.member` fhExts fh = Just fh
    | otherwise = findHandler e fhs
