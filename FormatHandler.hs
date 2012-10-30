{-# LANGUAGE Rank2Types #-}
module FormatHandler
    ( FormatHandler (..)
    , uriToText
    , findHandler
    ) where

import qualified Data.Set as Set
import qualified Data.Text as T
import Yesod.Core
import Yesod.Form
import Text.Blaze.Html (Html)
import Data.Conduit
import Data.Conduit.List (consume)
import qualified Data.ByteString as S
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.URI.Conduit
import Control.Monad (liftM)
import qualified Data.ByteString.Lazy as L
import Data.Maybe
import Prelude

data FormatHandler master = FormatHandler
    { fhExts :: Set.Set Ext
    , fhName :: T.Text
    , fhForm :: forall sub. RenderMessage master FormMessage => Maybe T.Text -> Html -> MForm sub master (FormResult T.Text, GWidget sub master ())
    , fhWidget :: forall sub. SchemeMap -> URI -> GWidget sub master ()
    , fhFilter :: L.ByteString -> Maybe (Source IO S.ByteString)
    , fhRefersTo :: SchemeMap -> URI -> IO [(URI, (Route master, [(T.Text, T.Text)]))]
    , fhTitle :: SchemeMap -> URI -> IO (Maybe T.Text)
    , fhFlatWidget :: forall sub. SchemeMap -> URI -> GWidget sub master ()
    , fhToText :: SchemeMap -> URI -> IO (Maybe T.Text)
    , fhExtraParents :: forall sub. SchemeMap -> URI -> GHandler sub master [(Maybe (Route master, [(T.Text, T.Text)]), T.Text)]
    }

type Ext = T.Text

-- uriToText :: MonadResource m => SchemeMap -> URI -> m T.Text
uriToText sm uri = liftM (decodeUtf8With lenientDecode . S.concat) $ runResourceT $ readURI sm uri $$ consume

findHandler :: Ext -> [FormatHandler m] -> Maybe (FormatHandler m)
findHandler _ [] = Nothing
findHandler e (fh:fhs)
    | e `Set.member` fhExts fh = Just fh
    | otherwise = findHandler e fhs
