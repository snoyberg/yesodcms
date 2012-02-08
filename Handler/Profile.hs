{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Profile
    ( getProfileR
    , postProfileR

    , postGroupsR
    , postDownGroupR
    , postUpGroupR
    , postDeleteGroupR

    , postLabelsR
    , postDeleteLabelR

    , getLabels
    ) where

import Foundation
import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.Text as T
import Yesod.Goodies.Gravatar
import Data.Monoid (mempty)
import Control.Monad (unless)
import Database.Persist.Query.Join hiding (runJoin)
import Database.Persist.Query.Join.Sql (runJoin)

form :: UserId -> User -> Form User
form uid user = renderTable $ User
    <$> pure (userEmail user)
    <*> aopt textField "Display name" (Just $ userName user)
    <*> areq (checkM unusedHandle textField) "Screen name" (Just $ userHandle user)
    <*> pure (userAdmin user)
  where
    unusedHandle handle = do
        mu <- runDB $ getBy $ UniqueHandle handle
        case mu of
            Just (Entity uid' _)
                | uid' /= uid -> return $ Left ("Username in use" :: T.Text)
            _ -> return $ Right handle

newLabelForm :: Form Label
newLabelForm nonce = do
    (name, nameview) <- mreq textField "Name" Nothing
    (group, groupview) <- mreq (selectField $ optionsPersist [] [Asc GroupPriority] groupName) "Group" Nothing
    return (Label <$> fmap entityKey group <*> name, [whamlet|
\#{nonce}
Create a field name ^{fvInput nameview} in the group ^{fvInput groupview}. #
<input type=submit value=Create>
|])

getProfileR :: Handler RepHtml
getProfileR = do
    Entity uid u <- requireAuth
    ((res, widget), enctype) <- runFormPost $ form uid u
    mgroups <-
        if userAdmin u
            then fmap Just $ runDB $ selectList [] [Asc GroupPriority]
            else return Nothing
    mlabels <-
        if userAdmin u
            then fmap Just $ runDB getLabels
            else return Nothing
    liftIO $ print (mlabels :: Maybe [(Entity Group, [Entity Label])])
    ((_, labelForm), _) <- runFormPost newLabelForm
    case res of
        FormSuccess u' -> do
            runDB $ replace uid u'
            setMessage "Profile updated"
            redirect ProfileR
        _ -> defaultLayout $(widgetFile "profile")
  where
    opts = defaultOptions
        { gDefault = Just Identicon
        , gSize = Just $ Size 160
        }

postProfileR :: Handler RepHtml
postProfileR = getProfileR

requireAdmin :: Handler ()
requireAdmin = do
    Entity _ u <- requireAuth
    unless (userAdmin u) $ permissionDenied "Only an admin can perform that action"

postGroupsR :: Handler ()
postGroupsR = do
    requireAdmin
    name <- runInputPost $ ireq textField "name"
    x <- runDB $ do
        x <- insertBy (Group name 1000)
        fixGroups
        return x
    case x of
        Left{} -> setMessage "Group with that name already exists"
        Right{} -> setMessage "Group created"
    redirect ProfileR

postUpGroupR, postDownGroupR, postDeleteGroupR :: GroupId -> Handler ()
postUpGroupR gid = do
    requireAdmin
    runDB $ update gid [GroupPriority -=. 15] >> fixGroups
    setMessage "Group moved up"
    redirect ProfileR
postDownGroupR gid = do
    requireAdmin
    runDB $ update gid [GroupPriority +=. 15] >> fixGroups
    setMessage "Group moved down"
    redirect ProfileR
postDeleteGroupR gid = do
    requireAdmin
    -- FIXME cascade
    runDB $ delete gid >> fixGroups
    setMessage "Group deleted"
    redirect ProfileR

fixGroups :: YesodDB Cms Cms ()
fixGroups = do
    keys <- fmap (map entityKey) $ selectList [] [Asc GroupPriority]
    mapM_ (\(key, priority) -> update key [GroupPriority =. priority]) $ zip keys [10, 20..]

postLabelsR :: Handler ()
postLabelsR = do
    requireAdmin
    ((res, _), _) <- runFormPost newLabelForm
    case res of
        FormSuccess label -> do
            x <- runDB $ insertBy label
            case x of
                Left{} -> setMessage "Label already exists"
                Right{} -> setMessage "New label created"
        _ -> setMessage "Invalid entry"
    redirect ProfileR

postDeleteLabelR :: LabelId -> Handler ()
postDeleteLabelR lid = do
    requireAdmin
    -- FIXME cascade
    runDB $ delete lid
    setMessage "Label deleted"
    redirect ProfileR

getLabels :: YesodDB Cms Cms [(Entity Group, [Entity Label])]
getLabels = runJoin (selectOneMany (LabelGroup <-.) labelGroup)
    { somOrderOne = [Asc GroupPriority]
    , somOrderMany = [Asc LabelName]
    }
