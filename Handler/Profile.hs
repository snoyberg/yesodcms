{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
module Handler.Profile
    ( getProfileR
    , postProfileR

    , postGroupsR
    , postDownGroupR
    , postUpGroupR
    , postDeleteGroupR

    , postLabelsR
    , postLabelsCsvR
    , postDeleteLabelR

    , getLabels
    ) where

import Foundation
import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.Text as T
import Data.Monoid (mempty)
import Control.Monad (unless, forM_)
import Database.Persist.Join hiding (runJoin)
import Database.Persist.Join.Sql (runJoin)
import Text.CSV
import qualified Data.ByteString.Lazy.Char8 as L8

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

newLabelForm :: Html -> Form Cms Cms (FormResult Label, Widget)
newLabelForm nonce = do
    (name, nameview) <- mreq textField "Name" Nothing
    (group, groupview) <- mreq (selectField' $ optionsPersist [] [Asc GroupPriority] groupName) "Group" Nothing
    return (Label <$> fmap fst group <*> name, [whamlet|
\#{nonce}
Create a field name ^{fvInput nameview} in the group ^{fvInput groupview}. #
<input type=submit value=Create>
|])

getProfileR :: Handler RepHtml
getProfileR = do
    (uid, u) <- requireAuth
    ((res, widget), enctype) <- runFormPost $ form uid u
    mgroups <-
        if userAdmin u
            then fmap Just $ runDB $ selectList [] [Asc GroupPriority]
            else return Nothing
    mlabels <-
        if userAdmin u
            then fmap Just $ runDB getLabels
            else return Nothing
    ((_, labelForm), _) <- runFormPost newLabelForm
    case res of
        FormSuccess u' -> do
            runDB $ replace uid u'
            setMessage "Profile updated"
            redirect RedirectTemporary ProfileR
        _ -> defaultLayout $(widgetFile "profile")

postProfileR :: Handler RepHtml
postProfileR = getProfileR

requireAdmin :: Handler ()
requireAdmin = do
    (_, u) <- requireAuth
    unless (userAdmin u) $ permissionDenied "Only an admin can perform that action"

postGroupsR :: Handler ()
postGroupsR = do
    requireAdmin
    name <- runInputPost $ ireq textField "name"
    x <- runDB $ do
        x <- insertBy (Group name 100000)
        fixGroups
        return x
    case x of
        Left{} -> setMessage "Group with that name already exists"
        Right{} -> setMessage "Group created"
    redirect RedirectTemporary ProfileR

postUpGroupR, postDownGroupR, postDeleteGroupR :: GroupId -> Handler ()
postUpGroupR gid = do
    requireAdmin
    runDB $ update gid [GroupPriority -=. 15] >> fixGroups
    setMessage "Group moved up"
    redirect RedirectTemporary ProfileR
postDownGroupR gid = do
    requireAdmin
    runDB $ update gid [GroupPriority +=. 15] >> fixGroups
    setMessage "Group moved down"
    redirect RedirectTemporary ProfileR
postDeleteGroupR gid = do
    requireAdmin
    -- FIXME cascade
    runDB $ delete gid >> fixGroups
    setMessage "Group deleted"
    redirect RedirectTemporary ProfileR

fixGroups :: YesodDB Cms Cms ()
fixGroups = do
    keys <- fmap (map fst) $ selectList [] [Asc GroupPriority]
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
    redirect RedirectTemporary ProfileR

postLabelsCsvR :: Handler ()
postLabelsCsvR = do
    requireAdmin
    (_, files) <- runRequestBody
    csvFile <- maybe (invalidArgs ["No file uploaded" :: T.Text]) return $ lookup "csv" files
    records <- either (invalidArgs . return . T.pack . show) return $ parseCSV "" $ L8.unpack $ fileContent csvFile
    runDB $ forM_ records $ \r ->
        case r of
            [g, l] -> do
                gid <- fmap (either fst id) $ insertBy $ Group (T.pack g) 100000
                fixGroups
                _ <- insertBy $ Label gid $ T.pack l
                return ()
            _ -> return ()
    setMessage "Groups and labels added"
    redirect RedirectTemporary ProfileR

postDeleteLabelR :: LabelId -> Handler ()
postDeleteLabelR lid = do
    requireAdmin
    -- FIXME cascade
    runDB $ delete lid
    setMessage "Label deleted"
    redirect RedirectTemporary ProfileR

getLabels :: YesodDB Cms Cms [((GroupId, Group), [(LabelId, Label)])]
getLabels = runJoin (selectOneMany (LabelGroup <-.) labelGroup)
    { somOrderOne = [Asc GroupPriority]
    , somOrderMany = [Asc LabelName]
    }
