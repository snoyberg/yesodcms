{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Blog
    ( getBlogR
    , getBlogPostNoDateR
    , getBlogPostR
    , getBlogArchiveR
    ) where

import Foundation
import Network.Gravatar
import Handler.Comments (prettyDateTime)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Arrow (second, (&&&))
import Data.List (groupBy)
import Data.Function (on)
import Handler.Feed (blogWidget)

getBlogR :: Handler ()
getBlogR = do
    Entity _ b <- runDB (selectFirst [] [Desc BlogPosted]) >>= maybe notFound return
    redirect $ BlogPostR (blogYear b) (blogMonth b) (blogSlug b)

getBlogPostNoDateR :: BlogSlugT -> Handler ()
getBlogPostNoDateR s = do
    Entity _ b <- runDB (selectFirst [BlogSlug ==. s] [Desc BlogPosted]) >>= maybe notFound return
    redirect $ BlogPostR (blogYear b) (blogMonth b) (blogSlug b)

type Year = Int
type Archive = [(Year, [(Month, [Entry])])]
data Entry = Entry
    { eTitle :: Text
    , eLink :: CmsRoute
    , eDate :: Text
    }

getBlogPostR :: Int -> Month -> BlogSlugT -> Handler RepHtml
getBlogPostR y m s = do
    Entity _ b <- runDB $ getBy404 $ UniqueBlog y m s
    u <- runDB $ get404 $ blogAuthor b
    let title = blogTitle b
    archive <- getArchive
    let current = Just $ BlogPostR y m s
    let blogArchive = $(widgetFile "blog-archive")
    let widget = blogWidget b
    defaultLayout $(widgetFile "blog")
  where
    opts = defaultOptions
        { gDefault = Just Identicon
        , gSize = Just $ Size 100
        }

toTuples :: Blog -> (Int, (Month, Entry))
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

getArchive :: Handler Archive
getArchive = fmap (map (second hoist) . hoist . map (toTuples . entityVal))
           $ runDB $ selectList [] [Desc BlogPosted]

getBlogArchiveR :: Handler RepHtml
getBlogArchiveR = do
    archive <- getArchive
    let current = Nothing
    let ba = $(widgetFile "blog-archive")
    defaultLayout [whamlet|<nav .toc>^{ba}|]
