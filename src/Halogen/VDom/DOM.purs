module Halogen.VDom.DOM
  ( VDomMachine
  , VDomStep
  , VDomSpec(..)
  , buildVDom
  , buildElem
  , buildKeyed
  , buildWidget
  ) where

import Prelude

import Control.Monad.Eff (Eff, foreachE)
import DOM (DOM)
import DOM.Node.Types (Element, Node, Document, elementToNode) as DOM
import Data.Array as Array
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Tuple (Tuple(..), fst)
import Halogen.VDom.Machine (Step(..), Machine)
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Types (ElemName(..), ElemSpec(..), Namespace(..), VDom(..), runGraft)
import Halogen.VDom.Util as Util

type VDomMachine eff a b = Machine (Eff eff) a b

type VDomStep eff a b = Eff eff (Step (Eff eff) a b)

-- | Widget machines recursively reference the configured spec to potentially
-- | enable recursive trees of Widgets.
newtype VDomSpec eff a w = VDomSpec
  { buildWidget ∷ VDomSpec eff a w → VDomMachine eff w DOM.Node
  , buildAttributes ∷ DOM.Element → VDomMachine eff a Unit
  , document ∷ DOM.Document
  , createElement :: ElemName -> DOM.Element
  , insertChildIx :: String -> Int -> DOM.Node -> DOM.Node -> Unit
  , removeChild :: DOM.Node -> DOM.Node -> Unit
  }

type VDomEffects eff = (dom ∷ DOM | eff)

-- | Starts an initial `VDom` machine by providing a `VDomSpec`.
-- |
-- | ```purescript
-- | main = do
-- |   machine1 ← buildVDom spec vdomTree1
-- |   machine2 ← Machine.step machine1 vdomTree2
-- |   machine3 ← Machine.step machine2 vdomTree3
-- |   ...
-- | ````
buildVDom
  ∷ ∀ eff a w
  . VDomSpec (VDomEffects eff) a w
  → VDomMachine (VDomEffects eff) (VDom a w) DOM.Node
buildVDom spec = render
  where
  render = case _ of
    Elem es ch → buildElem spec es ch
    Keyed es ch → buildKeyed spec es ch
    Widget w → buildWidget spec w
    Grafted g → buildVDom spec (runGraft g)

buildElem
  ∷ ∀ eff a w
  . VDomSpec (VDomEffects eff) a w
  → ElemSpec a
  → Array (VDom a w)
  → VDomStep (VDomEffects eff) (VDom a w) DOM.Node
buildElem (VDomSpec spec) = render
  where
  render es1@(ElemSpec ns1 name1 as1) ch1 = do
    let
      el = spec.createElement name1
      node = DOM.elementToNode el
      onChild = Fn.mkFn2 \ix child → do
        res@Step n m h ← buildVDom (VDomSpec spec) child
        pure $ spec.insertChildIx "render" ix n node
        pure res
    steps ← Fn.runFn2 Util.forE ch1 onChild
    attrs ← spec.buildAttributes el as1
    pure
      (Step node
        (Fn.runFn4 patch node attrs es1 steps)
        (Fn.runFn3 done node attrs steps))

  patch = Fn.mkFn4 \node attrs (es1@(ElemSpec ns1 name1 as1)) ch1 → case _ of
    Grafted g →
      Fn.runFn4 patch node attrs es1 ch1 (runGraft g)
    Elem es2@(ElemSpec ns2 name2 as2) ch2 | Fn.runFn2 eqElemSpec es1 es2 → do
      case Array.length ch1, Array.length ch2 of
        0, 0 → do
          attrs' ← Machine.step attrs as2
          pure
            (Step node
              (Fn.runFn4 patch node attrs' es2 ch1)
              (Fn.runFn3 done node attrs' ch1))
        _, _ → do
          let
            onThese = Fn.mkFn3 \ix (prev@Step n step halt) vdom → do
              res@Step n' m' h' ← step vdom
              pure $ spec.insertChildIx "patch" ix n' node
              pure res
            onThis = Fn.mkFn2 \ix (Step n _ halt) → halt
            onThat = Fn.mkFn2 \ix vdom → do
              res@Step n m h ← buildVDom (VDomSpec spec) vdom
              pure $ spec.insertChildIx "patch" ix n node
              pure res
          steps ← Fn.runFn5 Util.diffWithIxE ch1 ch2 onThese onThis onThat
          attrs' ← Machine.step attrs as2
          pure
            (Step node
              (Fn.runFn4 patch node attrs' es2 steps)
              (Fn.runFn3 done node attrs' steps))
    vdom → do
      Fn.runFn3 done node attrs ch1
      buildVDom (VDomSpec spec) vdom

  done = Fn.mkFn3 \node attrs steps → do
    parent ← pure (Util.unsafeParent node)
    pure $ spec.removeChild node parent
    foreachE steps Machine.halt
    Machine.halt attrs

buildKeyed
  ∷ ∀ eff a w
  . VDomSpec (VDomEffects eff) a w
  → ElemSpec a
  → Array (Tuple String (VDom a w))
  → VDomStep (VDomEffects eff) (VDom a w) DOM.Node
buildKeyed (VDomSpec spec) = render
  where
  render es1@(ElemSpec ns1 name1 as1) ch1 = do
    let
      el = spec.createElement name1
      node = DOM.elementToNode el
      onChild = Fn.mkFn3 \k ix (Tuple _ vdom) → do
        res@Step n m h ← buildVDom (VDomSpec spec) vdom
        pure $ spec.insertChildIx "render" ix n node
        pure res
    steps ← Fn.runFn3 Util.strMapWithIxE ch1 fst onChild
    attrs ← spec.buildAttributes el as1
    pure
      (Step node
        (Fn.runFn5 patch node attrs es1 steps (Array.length ch1))
        (Fn.runFn3 done node attrs steps))

  patch = Fn.mkFn5 \node attrs (es1@(ElemSpec ns1 name1 as1)) ch1 len1 → case _ of
    Grafted g →
      Fn.runFn5 patch node attrs es1 ch1 len1 (runGraft g)
    Keyed es2@(ElemSpec ns2 name2 as2) ch2 | Fn.runFn2 eqElemSpec es1 es2 →
      case len1, Array.length ch2 of
        0, 0 → do
          attrs' ← Machine.step attrs as2
          pure
            (Step node
              (Fn.runFn5 patch node attrs' es2 ch1 0)
              (Fn.runFn3 done node attrs' ch1))
        _, len2 → do
          let
            onThese = Fn.mkFn4 \k ix' (Step n step _) (Tuple _ vdom) → do
              res@Step n' m' h' ← step vdom
              pure $ spec.insertChildIx "patch" ix' n' node
              pure res
            onThis = Fn.mkFn2 \k (Step n _ halt) → halt
            onThat = Fn.mkFn3 \k ix (Tuple _ vdom) → do
              res@Step n' m' h' ← buildVDom (VDomSpec spec) vdom
              pure $ spec.insertChildIx "patch" ix n' node
              pure res
          steps ← Fn.runFn6 Util.diffWithKeyAndIxE ch1 ch2 fst onThese onThis onThat
          attrs' ← Machine.step attrs as2
          pure
            (Step node
              (Fn.runFn5 patch node attrs' es2 steps len2)
              (Fn.runFn3 done node attrs' steps))
    vdom → do
      Fn.runFn3 done node attrs ch1
      buildVDom (VDomSpec spec) vdom

  done = Fn.mkFn3 \node attrs steps → do
    parent ← pure (Util.unsafeParent node)
    pure $ spec.removeChild node parent
    Fn.runFn2 Util.forInE steps (Fn.mkFn2 \_ (Step _ _ halt) → halt)
    Machine.halt attrs

buildWidget
  ∷ ∀ eff a w
  . VDomSpec (VDomEffects eff) a w
  → w
  → VDomStep (VDomEffects eff) (VDom a w) DOM.Node
buildWidget (VDomSpec spec) = render
  where
  render w = do
    res@Step n m h ← spec.buildWidget (VDomSpec spec) w
    pure (Step n (patch res) h)

  patch prev@(Step node step halt) = case _ of
    Grafted g →
      patch prev (runGraft g)
    Widget w → do
      res@Step n m h ← step w
      pure (Step n (patch res) h)
    vdom → do
      halt
      buildVDom (VDomSpec spec) vdom

eqElemSpec
  ∷ ∀ a
  . Fn.Fn2 (ElemSpec a) (ElemSpec a) Boolean
eqElemSpec = Fn.mkFn2 \a b →
  case a, b of
    ElemSpec ns1 name1 _, ElemSpec ns2 name2 _ | name1 == name2 →
      case ns1, ns2 of
        Just (Namespace ns1'), Just (Namespace ns2') | ns1' == ns2' → true
        Nothing, Nothing → true
        _, _ → false
    _, _ → false
