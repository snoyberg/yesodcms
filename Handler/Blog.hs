{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Blog
    ( getBlogR
    , getBlogPostNoDateR
    , getBlogPostR
    ) where

import Foundation
import Yesod.Goodies.Gravatar
import Handler.Comments (prettyDateTime)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow (second, (&&&))
import Data.List (groupBy)
import Data.Function (on)
import FormatHandler
import FileStore

getBlogR :: Handler ()
getBlogR = do
    (_, b) <- runDB (selectFirst [] [Desc BlogPosted]) >>= maybe notFound return
    redirect RedirectTemporary $ BlogPostR (blogYear b) (blogMonth b) (blogSlug b)

getBlogPostNoDateR :: BlogSlugT -> Handler ()
getBlogPostNoDateR s = do
    (_, b) <- runDB (selectFirst [BlogSlug ==. s] [Desc BlogPosted]) >>= maybe notFound return
    redirect RedirectTemporary $ BlogPostR (blogYear b) (blogMonth b) (blogSlug b)

type Year = Int
type Archive = [(Year, [(Month, [Entry])])]
data Entry = Entry
    { eTitle :: Text
    , eLink :: CmsRoute
    , eDate :: Text
    }

getBlogPostR :: Int -> Month -> BlogSlugT -> Handler RepHtml
getBlogPostR y m s = do
    (_, b) <- runDB $ getBy404 $ UniqueBlog y m s
    u <- runDB $ get404 $ blogAuthor b
    let title = blogTitle b
    archive <- fmap (map (second hoist) . hoist . map (toTuples . snd)) $ runDB $ selectList [] [Desc BlogPosted]
    Cms { formatHandlers = fhs, fileStore = fs } <- getYesod
    let mfh = findHandler (snd $ T.breakOnEnd "." $ blogContents b) fhs
    muri <- liftIO $ fsGetFile fs $ blogContents b
    let widget =
            case (mfh, muri) of
                (Just fh, Just uri) -> fhFlatWidget fh (fsSM fs) uri
                _ -> [whamlet|<p>Format handler not found for #{blogContents b}|]
    defaultLayout $(widgetFile "blog")
  where
    opts = defaultOptions
        { gDefault = Just Identicon
        , gSize = Just $ Size 100
        }
    toTuples b = (blogYear b, (blogMonth b, Entry
        { eTitle = blogTitle b
        , eLink = BlogPostR (blogYear b) (blogMonth b) (blogSlug b)
        , eDate = prettyDateTime $ blogPosted b
        }))

hoist :: Eq a => [(a, b)] -> [(a, [b])]
hoist = map (fst . head &&& map snd) . groupBy ((==) `on` fst)

prettyMonth :: Month -> T.Text
prettyMonth (Month i) =
    months !! (i - 1)
  where
    months = T.words "January February March April May June July August September October November December"
