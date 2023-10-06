{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ClickToEdit
  ( clickToEdit,
  )
where

import Index (renderPage)
import Text.Blaze ((!))
import Text.Blaze.Html5 qualified as Html
import Text.Blaze.Html5.Attributes qualified as Html
  ( class_,
    name,
    type_,
    value,
  )
import Text.Blaze.Htmx qualified as Htmx
import Web.Scotty qualified as Scotty

clickToEdit :: Scotty.ScottyM ()
clickToEdit = do
  Scotty.get "/contact/1" $
    renderPage $
      clickEditInActiveForm

  Scotty.get "/contact/1/edit" $
    renderPage $
      clickEditActiveForm

  Scotty.put "/contact/1" $
    renderPage $
      clickEditInActiveForm

clickEditInActiveForm :: Html.Html
clickEditInActiveForm =
  Html.div ! Htmx.hxTarget "this" ! Htmx.hxSwap "outerHTML" $ do
    Html.div $ do
      Html.label (Html.toHtml @Text "First Name")
      Html.toHtml @Text ": Joe"

    Html.div $ do
      Html.label (Html.toHtml @Text "Last Name")
      Html.toHtml @Text ": Blow"

    Html.div $ do
      Html.label (Html.toHtml @Text "Email")
      Html.toHtml @Text ": joe@blow.com"

    Html.button
      ! Htmx.hxGet "/contact/1/edit"
      ! Html.class_ "btn btn-primary"
      $ Html.toHtml @Text "Click to Edit"

clickEditActiveForm :: Html.Html
clickEditActiveForm =
  Html.form
    ! Htmx.hxPut "/contact/1"
    ! Htmx.hxTarget "this"
    ! Htmx.hxSwap "outerHtml"
    $ do
      Html.div ! Html.class_ "form-group" $ do
        Html.label (Html.toHtml @Text "First Name")
        Html.input
          ! Html.type_ "text"
          ! Html.name "firstname"
          ! Html.value "Joe"

      Html.div ! Html.class_ "form-group" $ do
        Html.label (Html.toHtml @Text "Last Name")
        Html.input
          ! Html.type_ "text"
          ! Html.name "lastname"
          ! Html.value "Blow"

      Html.div ! Html.class_ "form-group" $ do
        Html.label (Html.toHtml @Text "Email")
        Html.input
          ! Html.type_ "text"
          ! Html.name "email"
          ! Html.value "joe@blow.com"

      Html.button ! Html.class_ "btn" $ Html.toHtml @Text "Submit"
      Html.button
        ! Html.class_ "btn"
        ! Htmx.hxGet "/contact/1"
        $ Html.toHtml @Text "Cancel"
