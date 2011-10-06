{-# LANGUAGE CPP, QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module Settings.StaticFiles where

import Yesod.Static
import qualified Yesod.Static as Static

static :: FilePath -> IO Static
#ifdef PRODUCTION
static = Static.static
#else
static = Static.staticDevel
#endif


-- | This generates easy references to files in the static directory at compile time.
--   The upside to this is that you have compile-time verification that referenced files
--   exist. However, any files added to your static directory during run-time can't be
--   accessed this way. You'll have to use their FilePath or URL to access them.
staticFilesList "static"
    [ "jquery.js"
    , "custom/list-remove.png"
    , "custom/list-add.png"
    , "css/normalize.css"
    , "custom/style.css"
    , "print.png"
    , "epub.png"
    , "pdf.png"
    , "ckeditor/ckeditor.js"
    , "ckeditor/adapters/jquery.js"
    , "feed.png"
    , "up.png"
    , "down.png"
    , "delete.png"
    ]
