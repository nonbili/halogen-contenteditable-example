module Example.Editor where

import Example.Prelude

import Data.Const (Const)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), isNothing)
import Data.Monoid as Monoid
import Data.Nullable as Nullable
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType(..))
import Web.HTML as Web
import Web.HTML.HTMLDocument as Document
import Web.HTML.HTMLElement (HTMLElement, DOMRect)
import Web.HTML.Window as Window

foreign import getSelectionRect_ :: EffectFn1 HTMLElement (Maybe DOMRect)
getSelectionRect :: HTMLElement -> Effect (Maybe DOMRect)
getSelectionRect el =
  (Nullable.toMaybe <<< unsafeCoerce) <$> runEffectFn1 getSelectionRect_ el

foreign import isSelectionLink :: Effect Boolean

foreign import execCommand_ :: EffectFn2 String String Unit
execCommand :: String -> String -> Effect Unit
execCommand = runEffectFn2 execCommand_

foreign import queryCommandState_ :: EffectFn1 String Boolean
queryCommandState :: String -> Effect Boolean
queryCommandState = runEffectFn1 queryCommandState_

type Query = Const Void

data Action
  = Init
  | OnSelectionChange
  | OnBold
  | OnItalic
  | OnLink
  | OnClear

type State =
  { selectionRect :: Maybe DOMRect
  , isBold :: Boolean
  , isItalic :: Boolean
  , isLink :: Boolean
  }

type HTML = H.ComponentHTML Action () Aff

initialState :: State
initialState =
  { selectionRect: Nothing
  , isBold: false
  , isItalic: false
  , isLink: false
  }

editorRef :: H.RefLabel
editorRef = H.RefLabel "editor"

renderPopup :: State -> DOMRect -> HTML
renderPopup state { left, top, width } =
  HH.div
  [ class_ "fixed bg-white shadow"
  , style sty
  ]
  [ HH.button
    [ class_ $
        btnCls <> Monoid.guard state.isBold activeCls
    , HE.onClick $ Just <<< const OnBold
    ]
    [ HH.text "bold" ]
  , HH.button
    [ class_ $
        btnCls <> Monoid.guard state.isItalic activeCls
    , HE.onClick $ Just <<< const OnItalic
    ]
    [ HH.text "italic" ]
  , HH.button
    [ class_ $
        btnCls <> Monoid.guard state.isLink activeCls
    , HE.onClick $ Just <<< const OnLink
    ]
    [ HH.text "link" ]
  , HH.button
    [ class_ btnCls
    , HE.onClick $ Just <<< const OnClear
    ]
    [ HH.text "clear" ]
  ]
  where
  activeCls = "text-blue-500"
  sty = String.joinWith ""
    [ "transform: translate(-50%, -100%);"
    , "left: " <> show (left + width / 2.0) <> "px;"
    , "top: " <> show top <> "px;"
    ]
  btnCls = ""

render :: State -> HTML
render state =
  HH.div_
  [ HH.h1
    [ class_ "text-lg font-bold mb-4"]
    [ HH.text "An editor example"]
  , HH.div_
    [ HH.button
      [ class_ $
          btnCls <> Monoid.guard (state.isBold && not btnDisabled) activeCls
      , HP.disabled btnDisabled
      , HE.onClick $ Just <<< const OnBold
      ]
      [ HH.text "bold" ]
    , HH.button
      [ class_ $
          btnCls <> Monoid.guard (state.isItalic && not btnDisabled) activeCls
      , HP.disabled btnDisabled
      , HE.onClick $ Just <<< const OnItalic
      ]
      [ HH.text "italic" ]
    , HH.button
      [ class_ $
          btnCls <> Monoid.guard (state.isLink && not btnDisabled) activeCls
      , HP.disabled btnDisabled
      , HE.onClick $ Just <<< const OnLink
      ]
      [ HH.text "link" ]
    , HH.button
      [ class_ btnCls
      , HP.disabled btnDisabled
      , HE.onClick $ Just <<< const OnClear
      ]
      [ HH.text "clear" ]
    ]
  , HH.div
    [ class_ "border p-3 overflow-y-auto"
    , style "min-height: 4rem; max-height: 60vh"
    , HH.attr (HH.AttrName "contenteditable") ""
    , HP.ref editorRef
    ]
    []
  , case state.selectionRect of
      Just rect -> renderPopup state rect
      Nothing -> HH.text ""
  ]
  where
  activeCls = "text-blue-500"
  btnDisabled = isNothing state.selectionRect
  btnCls =
    if btnDisabled
    then "text-gray-500"
    else ""

component :: H.Component HH.HTML Query Unit Void Aff
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      }
  }

handleAction :: Action -> H.HalogenM State Action () Void Aff Unit
handleAction = case _ of
  Init -> do
    doc <- H.liftEffect $ Web.window >>= Window.document
    void $ H.subscribe $
      ES.eventListenerEventSource (EventType "selectionchange") (Document.toEventTarget doc)
        (const $ Just OnSelectionChange)

  OnSelectionChange -> do
    H.getHTMLElementRef editorRef >>= traverse_ \el -> do
      selectionRect <- H.liftEffect $ getSelectionRect el
      isBold <- H.liftEffect $ queryCommandState "bold"
      isItalic <- H.liftEffect $ queryCommandState "italic"
      isLink <- H.liftEffect isSelectionLink
      H.modify_ $ _
        { selectionRect = selectionRect
        , isBold = isBold
        , isItalic = isItalic
        , isLink = isLink
        }

  OnBold -> H.liftEffect $ execCommand "bold" ""

  OnItalic -> H.liftEffect $ execCommand "italic" ""

  OnLink -> do
    state <- H.get
    H.liftEffect do
      if state.isLink
        then execCommand "unlink" ""
        else execCommand "createLink" " "

  OnClear -> H.liftEffect do
    execCommand "removeFormat" ""
    execCommand "unlink" ""
