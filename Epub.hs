{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Epub
    ( epub
    ) where

import Text.Blaze (Html)
import qualified Data.ByteString.Lazy as L
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Codec.Archive.Zip
import qualified Data.Digest.CRC32 as CRC32
import Text.Hamlet.XML (xml)
import qualified Text.XML as X
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 ()
import Text.HTML.TagSoup (parseTags, renderTags, Tag (TagOpen))

epub :: Html -> L.ByteString
epub html =
    fromArchive $ foldr addEntryToArchive emptyArchive entries
  where
    entries =
        [ mimetype
        , container
        , contentopf
        , tocncx
        , toEntry "index.html" 0 $ renderTags $ map addNS $ parseTags $ renderHtml html
        ]
    addNS (TagOpen "html" as) = TagOpen "html" $ ("xmlns", "http://www.w3.org/1999/xhtml") : as
    addNS t = t

-- | mimetype file must not be compressed, thus all the fancy footwork here.
mimetype :: Entry
mimetype = Entry
    { eRelativePath = "mimetype"
    , eCompressionMethod = NoCompression
    , eLastModified = 0
    , eCRC32 = CRC32.crc32 content'
    , eCompressedSize = size
    , eUncompressedSize = size
    , eExtraField = L.empty
    , eFileComment = L.empty
    , eInternalFileAttributes = 0
    , eExternalFileAttributes = 0
    , eCompressedData = content'
    }
  where
    content' = "application/epub+zip"
    size = fromIntegral $ L.length content'

container :: Entry
container =
    toEntry "META-INF/container.xml" 0 $ X.renderLBS X.def doc
  where
    doc = X.Document (X.Prologue [] Nothing []) root []
    root = X.Element
        "{urn:oasis:names:tc:opendocument:xmlns:container}container"
        [("version", "1.0")]
        [xml|
<{urn:oasis:names:tc:opendocument:xmlns:container}rootfiles>
    <{urn:oasis:names:tc:opendocument:xmlns:container}rootfile full-path=content.opf media-type=application/oebps-package+xml>
|]

contentopf :: Entry
contentopf =
    toEntry "content.opf" 0 $ X.renderLBS X.def doc
  where
    doc = X.Document (X.Prologue [] Nothing []) root []
    root = X.Element
        "{http://www.idpf.org/2007/opf}package"
        [("version", "2.0"), ("unique-identifier", "bookid")]
        [xml|
<{http://www.idpf.org/2007/opf}metadata>
    <{http://purl.org/dc/elements/1.1/}title>MyDocs
    <{http://purl.org/dc/elements/1.1/}identifier id=bookid>ditadocs-mydocs
    <{http://purl.org/dc/elements/1.1/}language>en
    <{http://www.idpf.org/2007/opf}meta name=cover content=cover-image>
<{http://www.idpf.org/2007/opf}manifest>
    <{http://www.idpf.org/2007/opf}item id=ncx href=toc.ncx media-type=application/x-dtbncx+xml>
    <{http://www.idpf.org/2007/opf}item id=index.html href=index.html media-type=application/xhtml+xml
<{http://www.idpf.org/2007/opf}spine toc=ncx>
    <{http://www.idpf.org/2007/opf}itemref idref=index.html>
<{http://www.idpf.org/2007/opf}guide>
    <{http://www.idpf.org/2007/opf}reference type=toc href=index.html title=Contents>
|]

tocncx :: Entry
tocncx =
    toEntry "toc.ncx" 0 $ X.renderLBS X.def doc
  where
    doc = X.Document (X.Prologue [] Nothing []) root []
    root = toNS "http://www.daisy.org/z3986/2005/ncx/" $ X.Element "ncx" [("version", "2005-1")] [xml|
<head>
    <meta name=fixme content=fixme>
<docTitle>
    <text>Ditadocs MyDocs
<navMap>
    <navPoint playOrder=1 id=index.html>
        <navLabel>
            <text>MyDocs
        <content src=index.html>
|]

-- | Force an elelment and all descendants into a namespace. Does not affect
-- attributes.
toNS :: T.Text -> X.Element -> X.Element
toNS namespace (X.Element name as ns) =
    X.Element name { X.nameNamespace = Just namespace } as $ map go ns
  where
    go (X.NodeElement e) = X.NodeElement $ toNS namespace e
    go n = n
