{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Profile
    ( getProfileR
    , postProfileR
    ) where

import Foundation
import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.Text as T
import Yesod.Goodies.Gravatar

form :: UserId -> User -> Html -> Form Cms Cms (FormResult User, Widget)
form uid user = renderTable $ User
    <$> pure (userEmail user)
    <*> aopt textField "Display name" (Just $ userName user)
    <*> areq (checkM unusedHandle textField) "Screen name" (Just $ userHandle user)
    <*> pure (userAdmin user)
  where
    unusedHandle handle = do
        mu <- runDB $ getBy $ UniqueHandle handle
        case mu of
            Just (uid', _)
                | uid' /= uid -> return $ Left ("Username in use" :: T.Text)
            _ -> return $ Right handle

getProfileR :: Handler RepHtml
getProfileR = do
    (uid, u) <- requireAuth
    ((res, widget), enctype) <- runFormPost $ form uid u
    case res of
        FormSuccess u' -> do
            runDB $ replace uid u'
            setMessage "Profile updated"
            redirect RedirectTemporary ProfileR
        _ -> defaultLayout $(widgetFile "profile")
  where
    opts = defaultOptions
        { gDefault = Just Identicon
        , gSize = Just $ Size 160
        }

postProfileR :: Handler RepHtml
postProfileR = getProfileR
