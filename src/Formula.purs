-- | A very simple forms library for Toestand
module Formula
  ( Text, viewText
  , Input, InputOn, viewInput, bindInput, bindInputOn
  , Checkbox, CheckboxOn, bindCheckbox, bindCheckboxOn ) where

import Prelude (Unit, bind, const, not, pure, void, ($), (<<<), (<$>))
import DOM.Simple.Event as DE
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T
import Record as Record
import Record.Builder as RB
import Type.Proxy (Proxy(..))
import Typisch.Row (class Cons, class Cons2, class Cons3, class Replaces)

type Text cell = ( text :: cell )

-- | Render some text from a view over a string
viewText :: forall cell. T.Read cell String => Record (Text cell) -> R.Element
viewText props = R.createElement viewTextCpt props []

viewTextCpt :: forall cell. T.Read cell String => R.Memo (Text cell)
viewTextCpt = R.memo' (R.hooksComponent "viewText" cpt) where
  cpt props@{ text } _ = H.text <$> T.useLive T.unequal text

type Builder l r = RB.Builder (Record l) (Record r)

type ChangeEvent = R.SyntheticEvent DE.MouseEvent

type BindChange r = ( change :: ChangeEvent -> Effect Unit | r )

type Input v r = ( value :: v | r )

type InputOn v r on = ( on :: Record on | Input v r )

type ViewInput r = ( defaultValue :: String | r )

type BindInput r on = ( on :: (Record (BindChange on)) | ViewInput r )

-- | Render an input component whose value is the value of the given
-- | view. Refreshes when the view is updated. Takes arbitrary extra
-- | properties to pass along to `H.input`
viewInput
  :: forall cell base
   . T.Read cell String
  => Replaces "value" cell "defaultValue" String base (Input cell base) (ViewInput base)
  => Record (Input cell base) -> R.Element
viewInput props = R.createElement viewInputCpt props []

viewInputCpt
  :: forall cell base
   . T.Read cell String
  => Replaces "value" cell "defaultValue" String base (Input cell base) (ViewInput base)
  => R.Memo (Input cell base)
viewInputCpt = R.memo' (R.hooksComponent "viewInput" cpt) where
  cpt :: Record (Input cell base) -> Array R.Element -> R.Hooks R.Element
  cpt props@{ value } _ = do
    val <- T.useLive T.unequal value
    pure $ H.input $ RB.build (adjust val) props
    where
      adjust :: String -> Builder (Input cell base) (ViewInput base)
      adjust val =  RB.insert defaultValueP val <<< RB.delete valueP
      valueP = Proxy :: Proxy "value"
      defaultValueP = Proxy :: Proxy "defaultValue"

-- | Render an input component whose value is the value of the given
-- | cursor. Refreshes when the view is updated. Changes to the input
-- | are reflected to the cursor. Takes arbitrary extra properties to
-- | pass along to `H.input`
bindInput
  :: forall cell base
   . T.ReadWrite cell String
  => Cons2 "defaultValue" String "on" (Record (BindChange ())) base (ViewInput base) (BindInput base ())
  => Cons "value" cell base (Input cell base)
  => Record (Input cell base) -> R.Element
bindInput props = R.createElement bindInputCpt props []

bindInputCpt
  :: forall cell base
   . T.ReadWrite cell String
  => Cons2 "defaultValue" String "on" (Record (BindChange ())) base (ViewInput base) (BindInput base ())
  => Cons "value" cell base (Input cell base)
  => R.Memo (Input cell base)
bindInputCpt = R.memo' (R.hooksComponent "bindInput" cpt) where
  cpt :: Record (Input cell base) -> Array R.Element -> R.Hooks R.Element
  cpt props@{ value } _ = do
    val <- T.useLive T.unequal value
    pure $ H.input $ RB.build (adjust val) props
    where
      adjust :: String -> Builder (Input cell base) (BindInput base ())
      adjust val = RB.insert onP { change } <<< RB.insert defaultValueP val <<< RB.delete valueP
      onP = Proxy :: Proxy "on"
      valueP = Proxy :: Proxy "value"
      defaultValueP = Proxy :: Proxy "defaultValue"
      change :: ChangeEvent -> Effect Unit
      change e = void $ T.write (R.unsafeEventValue e) value

-- | Like `bindInput`, but allows you to pass 'on'
bindInputOn
  :: forall cell base on props' inner'
   . T.ReadWrite cell String
  => Cons2 "on" (Record on)              "value"        cell   base props' (InputOn cell base on)
  => Cons2 "on" (Record (BindChange on)) "defaultValue" String base inner' (BindInput base on)
  => Cons "change" (ChangeEvent -> Effect Unit) on (BindChange on)
  => Record (InputOn cell base on) -> R.Element
bindInputOn props = R.createElement bindInputOnCpt props []

bindInputOnCpt
  :: forall cell base on props' inner'
   . T.ReadWrite cell String
  => Cons2 "on" (Record on)              "value"        cell   base props' (InputOn cell base on)
  => Cons2 "on" (Record (BindChange on)) "defaultValue" String base inner' (BindInput base on)
  => Cons "change" (ChangeEvent -> Effect Unit) on (BindChange on)
  => R.Memo (InputOn cell base on)
bindInputOnCpt = R.memo' (R.hooksComponent "bindInputOn" cpt) where
  cpt :: Record (InputOn cell base on) -> Array R.Element -> R.Hooks R.Element
  cpt props@{ value } _ = do
    val <- T.useLive T.unequal value
    pure $ H.input (RB.build (adjust val) props)
    where
      adjust :: String -> Builder (InputOn cell base on) (BindInput base on)
      adjust val = RB.modify onP (Record.insert changeP change) <<< RB.insert defaultValueP val <<< RB.delete valueP
      onP = Proxy :: Proxy "on"
      valueP = Proxy :: Proxy "value"
      changeP = Proxy :: Proxy "change"
      defaultValueP = Proxy :: Proxy "defaultValue"
      change :: ChangeEvent -> Effect Unit
      change e = void $ T.write (R.unsafeEventValue e) value

type Checkbox v r = ( checked :: v | r )

type CheckboxOn v r on = ( on :: Record on | Checkbox v r )

type BindCheckbox r on = ( on :: (Record (BindChange on)), type :: String | Checkbox Boolean r )

-- | Render a checkbox component whose value is the value of the given
-- | cursor. Refreshes when the view is updated. Changes to the input
-- | are reflected to the cursor. Takes arbitrary extra properties to
-- | pass along to `H.input`
bindCheckbox
  :: forall cell base
   . T.ReadWrite cell Boolean
  => Cons3 "checked" Boolean
           "on"      (Record (BindChange ()))
           "type"    String
           base
           (Checkbox Boolean base)
           (CheckboxOn Boolean base (BindChange ()))
           (BindCheckbox base ())
  => Cons  "checked" cell base (Checkbox cell base)
  => Record (Checkbox cell base) -> R.Element
bindCheckbox props = R.createElement bindCheckboxCpt props []

bindCheckboxCpt
  :: forall cell base
   . T.ReadWrite cell Boolean
  => Cons3 "checked" Boolean
           "on"      (Record (BindChange ()))
           "type"    String
           base
           (Checkbox Boolean base)
           (CheckboxOn Boolean base (BindChange ()))
           (BindCheckbox base ())
  => Cons  "checked" cell base (Checkbox cell base)
  => R.Memo (Checkbox cell base)
bindCheckboxCpt = R.memo' (R.hooksComponent "bindCheckbox" cpt) where
  cpt :: Record (Checkbox cell base) -> Array R.Element -> R.Hooks R.Element
  cpt props@{ checked } _ = do
    val <- T.useLive T.unequal checked
    pure $ H.input $ RB.build (adjust val) props
    where
      adjust :: Boolean -> Builder (Checkbox cell base) (BindCheckbox base ())
      adjust val = addType <<< addOn <<< changeChecked where
        change :: ChangeEvent -> Effect Unit
        change e = void $ T.write (not val) checked
        addType = RB.insert (Proxy :: Proxy "type") "checkbox"
        addOn   = RB.insert (Proxy :: Proxy "on") { change }
        changeChecked = RB.modify (Proxy :: Proxy "checked") (const val)

-- | Like `bindCheckbox` but allows you to add to the 'on' prop
bindCheckboxOn
  :: forall cell base on inner'
   . T.ReadWrite cell Boolean
  => Cons3 "checked" Boolean
           "on"      (Record (BindChange on))
           "type"    String
           base
           (Checkbox Boolean base)
           (CheckboxOn Boolean base (BindChange on))
           (BindCheckbox base on)
  => Cons2 "on" (Record (BindChange on)) "checked" Boolean base inner' (BindCheckbox base on)
  => Cons "change" (ChangeEvent -> Effect Unit) on (BindChange on)
  => Record (CheckboxOn cell base on) -> R.Element
bindCheckboxOn props = R.createElement bindCheckboxOnCpt props []

bindCheckboxOnCpt
  :: forall cell base on inner'
   . T.ReadWrite cell Boolean
  => Cons3 "checked" Boolean
           "on"      (Record (BindChange on))
           "type"    String
           base
           (Checkbox Boolean base)
           (CheckboxOn Boolean base (BindChange on))
           (BindCheckbox base on)
  => Cons2 "on" (Record (BindChange on)) "checked" Boolean base inner' (BindCheckbox base on)
  => Cons "change" (ChangeEvent -> Effect Unit) on (BindChange on)
  => R.Memo (CheckboxOn cell base on)
bindCheckboxOnCpt = R.memo' (R.hooksComponent "bindCheckbox'" cpt) where
  cpt :: Record (CheckboxOn cell base on) -> Array R.Element -> R.Hooks R.Element
  cpt props@{ checked } _ = do
    val <- T.useLive T.unequal checked
    pure $ H.input (RB.build (adjust val) props)
    where
      adjust :: Boolean -> Builder (CheckboxOn cell base on) (BindCheckbox base on)
      adjust val = addType <<< changeOn <<< changeChecked where
        change :: ChangeEvent -> Effect Unit
        change e = void $ T.write (not val) checked
        addType = RB.insert (Proxy :: Proxy "type") "checkbox"
        changeChecked = RB.modify (Proxy :: Proxy "checked") (const val)
        changeOn  = RB.modify (Proxy :: Proxy "on") (Record.insert changeP change) where
          changeP = Proxy :: Proxy "change"
