{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module BulkUpdate (bulkUpdate, usersDatabase) where

import Data.Text qualified as Text
import Index (renderPage, renderPageWithIndex)
import Text.Blaze ((!))
import Text.Blaze.Html5 qualified as Html
import Text.Blaze.Html5.Attributes qualified as Html
  ( class_,
    id,
    name,
    type_,
    value,
  )
import Text.Blaze.Htmx qualified as Htmx
import Web.Scotty qualified as Scotty

bulkUpdate :: IORef [User] -> Scotty.ScottyM ()
bulkUpdate usersRef = do
  Scotty.get "/" $ do
    users <- readIORef usersRef
    renderPageWithIndex $
      bulkUpdateForm users

  Scotty.put "/activate" $ do
    params <- Scotty.formParams
    userUpdate <- lift $ updateUsers usersRef params Active
    renderPage (userTable userUpdate)

  Scotty.put "/deactivate" $ do
    params <- Scotty.formParams
    userUpdate <- lift $ updateUsers usersRef params Inactive
    renderPage (userTable userUpdate)

data User = User
  { id :: !Int,
    name :: !Text,
    email :: !Text,
    status :: !Status
  }
  deriving stock (Eq, Show)

data Status = Active | Inactive
  deriving stock (Eq, Show)

instance ToText Status where
  toText Active = "Active"
  toText Inactive = "Inactive"

usersDatabase :: IO (IORef [User])
usersDatabase = newIORef users
  where
    users :: [User]
    users =
      [ User
          { id = 0,
            name = "Joe Smith",
            email = "joe@smith.org",
            status = Active
          },
        User
          { id = 1,
            name = "Angie McDowell",
            email = "angie@macdowell.org",
            status = Active
          },
        User
          { id = 2,
            name = "Fuqua Tarkenton",
            email = "fuqua@tarkenton.org",
            status = Active
          },
        User
          { id = 3,
            name = "Kim Yee",
            email = "kim@yee.org",
            status = Inactive
          }
      ]

updateUsers :: IORef [User] -> [Scotty.Param] -> Status -> IO [User]
updateUsers usersRef request newStatus =
  atomicModifyIORef usersRef $ \users ->
    let newUsers =
          users <&> \user ->
            if user.id `elem` ((.id) <$> (paramsToUsers request users))
              then user {status = newStatus}
              else user
     in (newUsers, newUsers)
  where
    paramsToUsers :: [Scotty.Param] -> [User] -> [User]
    paramsToUsers request users =
      let ids = catMaybes $ (\(_k, value) -> readMaybe @Int $ Text.unpack value) <$> request
       in filter (\user -> user.id `elem` ids) users

bulkUpdateForm :: [User] -> Html.Html
bulkUpdateForm users = do
  Html.form ! Html.id "checked-contacts" $
    Html.table $ do
      Html.thead $
        Html.tr $ do
          Html.th $ mempty
          Html.th $ (Html.toHtml @Text "Name")
          Html.th $ (Html.toHtml @Text "Email")
          Html.th $ (Html.toHtml @Text "Status")

      Html.tbody ! Html.id "tbody" $
        userTable users

  Html.br
  Html.br

  Html.div ! Htmx.hxInclude "#checked-contacts" ! Htmx.hxTarget "#tbody" $ do
    Html.button
      ! Html.class_ "btn"
      ! Htmx.hxPut "/activate"
      $ Html.toHtml @Text "Activate"
    Html.button
      ! Html.class_ "btn"
      ! Htmx.hxPut "/deactivate"
      ! Html.value "deactivate"
      $ Html.toHtml @Text "Deactivate"

userTable :: [User] -> Html.Html
userTable users =
  forM_ users $ \user ->
    Html.tr ! Html.class_ mempty $ do
      Html.td $
        Html.input
          ! Html.type_ "checkbox"
          ! Html.name "ids"
          ! Html.value (fromString $ show user.id)
      Html.td $ Html.toHtml user.name
      Html.td $ Html.toHtml user.email
      Html.td $ Html.toHtml (toText user.status)
