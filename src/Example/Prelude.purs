module Example.Prelude
  ( class_
  , style
  , module Prelude
  ) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

class_ :: forall r i. String -> HH.IProp ("class" :: String | r) i
class_ = HP.class_ <<< HH.ClassName

style :: forall r i. String -> HH.IProp ("style" :: String | r) i
style = HH.attr (HH.AttrName "style")
