module Halogen.VDom.Util
  ( newMutMap
  , pokeMutMap
  , deleteMutMap
  , unsafeFreeze
  , unsafeLookup
  , unsafeGetAny
  , unsafeGetProp
  , unsafeHasAny
  , unsafeSetAny
  , unsafeSetProp
  , unsafeDeleteAny
  , forE
  , forEachE
  , forInE
  , replicateE
  , diffWithIxE
  , diffWithKeyAndIxE
  , diffPropWithKeyAndIxE
  , strMapWithIxE
  , refEq
  , createTextNode
  , setTextContent
  , createElement
  , createMicroapp
  , insertChildIx
  , generateUUID
  , removeChild
  , parentNode
  , setAttribute
  , removeAttribute
  , addEventListener
  , removeEventListener
  , removeProperty
  , JsUndefined
  , jsUndefined
  , generateUUID
  ) where

import Prelude

import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Uncurried as EFn
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.Object.ST (STObject)
import Foreign.Object.ST as STObject
import Halogen.VDom.Types (ElemName, Namespace, FnObject)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document) as DOM
import Web.DOM.Element (Element) as DOM
import Web.DOM.Node (Node) as DOM
import Web.Event.EventTarget (EventListener) as DOM

newMutMap ∷ ∀ r a. Effect (STObject r a)
newMutMap = unsafeCoerce STObject.new

pokeMutMap ∷ ∀ r a. EFn.EffectFn3 String a (STObject r a) Unit
pokeMutMap = unsafeSetAny

deleteMutMap ∷ ∀ r a. EFn.EffectFn2 String (STObject r a) Unit
deleteMutMap = unsafeDeleteAny

unsafeFreeze ∷ ∀ r a. STObject r a → Object a
unsafeFreeze = unsafeCoerce

unsafeLookup ∷ ∀ a. Fn.Fn2 String (Object a) a
unsafeLookup = unsafeGetAny

foreign import unsafeGetProp
  ∷ ∀ a b. Fn.Fn2 String a b

foreign import unsafeGetAny
  ∷ ∀ a b. Fn.Fn2 String a b

foreign import unsafeHasAny
  ∷ ∀ a. Fn.Fn2 String a Boolean

foreign import unsafeSetProp
  ∷ ∀ a b. EFn.EffectFn3 String a b Unit

foreign import unsafeSetAny ∷ ∀ a b. EFn.EffectFn3 String a b Unit

foreign import removeProperty
  ∷ ∀ a b. EFn.EffectFn3 String a b Unit

foreign import unsafeDeleteAny
  ∷ ∀ a. EFn.EffectFn2 String a Unit

foreign import forE
  ∷ ∀ a b
  . EFn.EffectFn2
      (Array a)
      (EFn.EffectFn2 Int a b)
      (Array b)

foreign import forEachE
  ∷ ∀ a
  . EFn.EffectFn2
      (Array a)
      (EFn.EffectFn1 a Unit)
      Unit

foreign import forInE
  ∷ ∀ a
  . EFn.EffectFn2
      (Object.Object a)
      (EFn.EffectFn2 String a Unit)
      Unit

foreign import replicateE
  ∷ ∀ a
  . EFn.EffectFn2
      Int
      (Effect a)
      Unit

foreign import diffWithIxE
  ∷ ∀ b c d
  . EFn.EffectFn5
      (Array b)
      (Array c)
      (EFn.EffectFn3 Int b c d)
      (EFn.EffectFn2 Int b Unit)
      (EFn.EffectFn2 Int c d)
      (Array d)

foreign import diffWithKeyAndIxE
  ∷ ∀ a b c d
  . EFn.EffectFn6
      (Object.Object a)
      (Array b)
      (b → String)
      (EFn.EffectFn4 String Int a b c)
      (EFn.EffectFn2 String a d)
      (EFn.EffectFn3 String Int b c)
      (Object.Object c)

foreign import diffPropWithKeyAndIxE
  ∷ ∀ a b c d el
  . EFn.EffectFn8
      FnObject
      (Object.Object a)
      (Array b)
      (b → String)
      (EFn.EffectFn4 String Int a b c)
      (EFn.EffectFn2 String a d)
      (EFn.EffectFn3 String Int b c)
      el
      (Object.Object c)

foreign import strMapWithIxE
  ∷ ∀ a b
  . EFn.EffectFn3
      (Array a)
      (a → String)
      (EFn.EffectFn3 String Int a b)
      (Object.Object b)

foreign import refEq
  ∷ ∀ a b. Fn.Fn2 a b Boolean

foreign import createTextNode
  ∷ EFn.EffectFn1 String  DOM.Node

foreign import setTextContent
  ∷ EFn.EffectFn2 String DOM.Node Unit

foreign import createElement
  ∷ EFn.EffectFn3 FnObject (Nullable Namespace) ElemName DOM.Element

foreign import createMicroapp
  ∷ EFn.EffectFn3 FnObject String String DOM.Element

foreign import generateUUID :: Effect String

foreign import insertChildIx
  ∷ EFn.EffectFn5 FnObject String Int DOM.Node DOM.Node Unit

foreign import removeChild
  ∷ EFn.EffectFn3 FnObject DOM.Node DOM.Node Unit

foreign import parentNode
  ∷ EFn.EffectFn1 DOM.Node DOM.Node

foreign import setAttribute
  ∷ EFn.EffectFn4 (Nullable Namespace) String String DOM.Element Unit

foreign import removeAttribute
  ∷ EFn.EffectFn3 (Nullable Namespace) String DOM.Element Unit

foreign import addEventListener
  ∷ EFn.EffectFn5 FnObject String String DOM.EventListener DOM.Element Unit

foreign import removeEventListener
  ∷ EFn.EffectFn3 String DOM.EventListener DOM.Element Unit

foreign import data JsUndefined ∷ Type

foreign import jsUndefined ∷ JsUndefined

