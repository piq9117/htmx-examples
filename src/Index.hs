{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Index
  ( index,
    renderPage,
  )
where

import Text.Blaze ((!))
import Text.Blaze.Html.Renderer.Text qualified as Html
import Text.Blaze.Html5 qualified as Html
import Text.Blaze.Html5.Attributes qualified as Html
  ( class_,
    src,
  )
import Text.Blaze.Internal qualified as Html
import Web.Scotty qualified as Scotty

index :: Html.Html -> Html.Html
index html = Html.docTypeHtml $ do
  Html.head $ do
    Html.script
      ! Html.src "https://unpkg.com/htmx.org@1.9.6"
      ! integrity "sha384-FhXw7b6AlE/jyjlZH5iHa/tTe9EpJ1Y55RjcgPbjeWMskSxZt1v9qkxLJWNJaGni"
      ! crossOrigin "anonymous"
      $ Html.toHtml @Text ""
    Html.title $ Html.toMarkup @Text "HTMX Examples"
  Html.body $
    Html.div ! Html.class_ "main" $
      html

integrity :: Html.AttributeValue -> Html.Attribute
integrity = Html.attribute "integrity" "integrity=\""

crossOrigin :: Html.AttributeValue -> Html.Attribute
crossOrigin = Html.attribute "crossOrigin" "crossorigin=\""

renderPage :: Html.Html -> Scotty.ActionM ()
renderPage = Scotty.html <<< Html.renderHtml <<< index
