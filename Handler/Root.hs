{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Root
    ( getRootR
    ) where

import Foundation
import Handler.Page (getPageR')

getRootR :: Handler RepHtml
getRootR = getPageR' []
