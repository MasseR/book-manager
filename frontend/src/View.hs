{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards   #-}
module View where

import           GHC.Generics (Generic)
import           Miso
import Data.Profunctor

data Page model a =
  Page { header  :: model -> View a
       , content :: model -> View a
       , footer  :: model -> View a }
                  deriving (Generic, Functor)

instance Profunctor Page where
  dimap f g Page{..} =
    Page { header = fmap g . header . f
         , content = fmap g . content . f
         , footer = fmap g . content . f }

baseView :: Page model a
baseView = Page { .. }
  where
    header _model = div_ [] [text "header"]
    content _model = div_ [] [text "content"]
    footer _model = div_ [] [text "footer"]

renderView :: Page model a -> model -> View a
renderView Page{..} model =
  div_ [] [ div_ [class_ "header"] [ header model ]
          , div_ [class_ "content"] [ content model ]
          , div_ [class_ "footer"] [ footer model ] ]
