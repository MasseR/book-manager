{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module View where

import           Data.Profunctor
import           GHC.Generics    (Generic)
import           Miso

data Page model a =
  Page { header  :: model -> [View a]
       , content :: model -> [View a]
       , footer  :: model -> [View a] }
                  deriving (Generic, Functor)

instance Semigroup (Page model action) where
  l <> r = Page { header = \m -> header l m <> header r m
                , content = \m -> content l m <> content r m
                , footer = \m -> footer l m <> footer r m
                }


instance Profunctor Page where
  rmap = fmap
  lmap g Page{..} =
    Page { header = header . g
         , content = content . g
         , footer = footer . g
         }

renderView :: Page model a -> model -> View a
renderView Page{..} model =
  div_ [] [ div_ [class_ "header"] ( header model )
          , div_ [class_ "content"] ( content model )
          , div_ [class_ "footer"] ( footer model ) ]
