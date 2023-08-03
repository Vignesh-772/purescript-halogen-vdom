module Halogen.VDom.DOM
  ( VDomSpec(..)
  , buildVDom
  , buildText
  , buildElem
  , buildKeyed
  , buildWidget
  ) where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toNullable)
import Data.Tuple (Tuple(..), fst)
import Effect.Uncurried as EFn
import Foreign (Foreign)
import Foreign.Object as Object
import Halogen.VDom.DOM.Prop (Prop(..))
import Halogen.VDom.Machine (Machine, Step, Step'(..), extract, halt, mkStep, step, unStep)
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Types (ElemName(..), FnObject, Namespace(..), ShimmerHolder, State1, State2(..), VDom(..), runGraft)
import Halogen.VDom.Util as Util
import Effect.Uncurried (EffectFn3, mkEffectFn3, runEffectFn1, runEffectFn3)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element) as DOM
import Web.DOM.Element as DOMElement
import Web.DOM.Node (Node) as DOM

type VDomMachine a w = Machine (VDom a w) DOM.Node

type VDomStep a w = Step (VDom a w) DOM.Node

type VDomInit i a w = EFn.EffectFn1 i (VDomStep a w)

type VDomBuilder i a w = EFn.EffectFn3 (VDomSpec a w) (VDomMachine a w) i (VDomStep a w)

type VDomBuilder4 i j k l a w = EFn.EffectFn6 (VDomSpec a w) (VDomMachine a w) i j k l (VDomStep a w)

type VDomBuilder5 i j a w ch = EFn.EffectFn5 (VDomSpec a w) (VDomMachine a w) i j ch (VDomStep a w)

type VDomBuilder6 i j a w = EFn.EffectFn4 (VDomSpec a w) (VDomMachine a w) i j (VDomStep a w)

-- | Widget machines recursively reference the configured spec to potentially
-- | enable recursive trees of Widgets.
newtype VDomSpec a w = VDomSpec
  { buildWidget ∷ VDomSpec a w → Machine w DOM.Node
  , buildAttributes ∷ FnObject -> DOM.Element → Machine a Unit
  , fnObject :: FnObject
  }

-- | Starts an initial `VDom` machine by providing a `VDomSpec`.
-- |
-- | ```purescript
-- | main = do
-- |   machine1 ← buildVDom spec vdomTree1
-- |   machine2 ← Machine.step machine1 vdomTree2
-- |   machine3 ← Machine.step machine2 vdomTree3
-- |   ...
-- | ````
buildVDom ∷ ∀ a w. VDomSpec a w → VDomMachine a w
buildVDom spec = build
  where
  build = EFn.mkEffectFn1 case _ of
    Text s → EFn.runEffectFn3 buildText spec build s
    Elem ns n a ch → EFn.runEffectFn6 buildElem spec build ns n a ch
    Chunk ns n a ch → EFn.runEffectFn6 buildChunk spec build ns n a ch
    Keyed ns n a ch → EFn.runEffectFn6 buildKeyed spec build ns n a ch
    Widget w → EFn.runEffectFn3 buildWidget spec build w
    PartialLayout (p) -> EFn.runEffectFn3 buildPartialLayout spec build p
    Grafted g → EFn.runEffectFn1 build (runGraft g)
    Microapp s g ch → EFn.runEffectFn5 buildMicroapp spec build s g ch

type MicroAppState a w =
  { build ∷ VDomMachine a w
  , node ∷ DOM.Node
  , requestId :: String
  , attrs ∷ Step a Unit
  , service :: String
  , payload :: Maybe Foreign
  , children :: Array (VDomStep a w)
  }

buildPartialLayout :: ∀ a w. EffectFn3 (VDomSpec a w) (VDomMachine a w) (State1 a w) (VDomStep a w)
buildPartialLayout = EFn.mkEffectFn3 \s@(VDomSpec spec) build partial  -> do
  let (State2 st fn) =  (unsafeCoerce partial)
      e = fn st
  case e of
    Elem ns1 name1 as1 childrens → do
      let ch1 = (Array.mapWithIndex (\i a -> Tuple (show i) a) childrens)
      el ← EFn.runEffectFn4 Util.createElement spec.fnObject (toNullable ns1) name1 "keyed"
      let
        node = DOMElement.toNode el
        onChild = EFn.mkEffectFn4 \k ix _ (Tuple _ vdom) → do
          res ← EFn.runEffectFn1 build vdom
          EFn.runEffectFn6 Util.insertChildIx spec.fnObject "render" ix (extract res) node k
          pure res

      -- Visibility Check logic
        -- loop on children to find which nodes to eliminate
        -- eliminate nodes where there is property visibility with value gone
      let ch2 = filterGoneNodes ch1

      children ← EFn.runEffectFn3 Util.strMapWithIxE ch2 fst onChild
      attrs ← EFn.runEffectFn1 (spec.buildAttributes spec.fnObject el) as1
      let
        state =
          { build
          , node
          , attrs
          , ns: ns1
          , name: name1
          , children
          , length: Array.length ch2
          }

      pure $ mkStep $ Step node state (patchPartial (unsafeCoerce partial) spec.fnObject) (haltKeyed spec.fnObject)
    Text str → EFn.runEffectFn3 buildText s build str
    Chunk ns n a ch → EFn.runEffectFn6 buildChunk s build ns n a ch
    Keyed ns1 name1 as1 ch1 → do
      el ← EFn.runEffectFn4 Util.createElement spec.fnObject (toNullable ns1) name1 "keyed"
      let
        node = DOMElement.toNode el
        onChild = EFn.mkEffectFn4 \k ix _ (Tuple _ vdom) → do
          res ← EFn.runEffectFn1 build vdom
          EFn.runEffectFn6 Util.insertChildIx spec.fnObject "render" ix (extract res) node k
          pure res

      -- Visibility Check logic
        -- loop on children to find which nodes to eliminate
        -- eliminate nodes where there is property visibility with value gone
      let ch2 = filterGoneNodes ch1

      children ← EFn.runEffectFn3 Util.strMapWithIxE ch2 fst onChild
      attrs ← EFn.runEffectFn1 (spec.buildAttributes spec.fnObject el) as1
      let
        state =
          { build
          , node
          , attrs
          , ns: ns1
          , name: name1
          , children
          , length: Array.length ch2
          }

      pure $ mkStep $ Step node state (patchPartial (unsafeCoerce partial) spec.fnObject) (haltKeyed spec.fnObject)
    Widget w → EFn.runEffectFn3 buildWidget s build w
    PartialLayout _ -> runEffectFn1 build e
    Grafted g → EFn.runEffectFn1 build (runGraft g)
    Microapp str g ch → EFn.runEffectFn5 buildMicroapp s build str g ch

patchPartialEvaluated :: ∀ a w. EFn.EffectFn3 (State1 a w) FnObject (KeyedState a w) (VDomStep a w)
patchPartialEvaluated = mkEffectFn3 \newPartial fnObject state -> do
  let (State2 newSt fn) = (unsafeCoerce newPartial)
      vdom = fn newSt
  let { build, node, attrs, ns: ns1, name: name1, children: ch1, length: len1 } = state
  let patchFunc = EFn.mkEffectFn4 \ns2 name2 as2 ch0 ->  do
        let ch2 = filterGoneNodes ch0
        case len1, Array.length ch2 of
          0, 0 → do
            attrs2 ← EFn.runEffectFn2 Machine.step attrs as2
            let
              nextState =
                { build
                , node
                , attrs: attrs2
                , ns: ns2
                , name: name2
                , children: ch1
                , length: 0
                }
            pure $ mkStep $ Step node nextState (patchPartial (unsafeCoerce newPartial) fnObject) (haltKeyed fnObject)
          _, len2 → do
            let
              onThese = EFn.mkEffectFn5 \obj k ix' s (Tuple _ v) → do
                res ← EFn.runEffectFn2 step s v
                EFn.runEffectFn6 Util.insertChildIx obj "patch" ix' (extract res) node k
                pure res
              onThis = EFn.mkEffectFn3 \_ _ s → EFn.runEffectFn1 halt s
              onThat = EFn.mkEffectFn4 \obj k ix (Tuple _ v) → do
                res ← EFn.runEffectFn1 build v
                EFn.runEffectFn6 Util.insertChildIx obj "patch" ix (extract res) node k
                pure res
            children2 ← EFn.runEffectFn7 Util.diffWithKeyAndIxE fnObject ch1 ch2 fst onThese onThis onThat
            attrs2 ← EFn.runEffectFn2 step attrs as2
            let
              nextState =
                { build
                , node
                , attrs: attrs2
                , ns: ns2
                , name: name2
                , children: children2
                , length: len2
                }
            pure $ mkStep $ Step node nextState (patchPartial (unsafeCoerce newPartial) fnObject) (haltKeyed fnObject)

  case vdom of
    Grafted g →
      EFn.runEffectFn2 (patchKeyed fnObject) state (runGraft g)
    Elem ns2 name2 as2 ch3 | Fn.runFn4 eqElemSpec ns1 name1 ns2 name2 →
      EFn.runEffectFn4 patchFunc ns2 name2 as2 (Array.mapWithIndex (\i a -> Tuple (show i) a) ch3)
    Keyed ns2 name2 as2 ch2 | Fn.runFn4 eqElemSpec ns1 name1 ns2 name2 →
      EFn.runEffectFn4 patchFunc ns2 name2 as2 ch2
    _ → do
      EFn.runEffectFn1 (haltKeyed fnObject) state
      EFn.runEffectFn1 build vdom


patchPartial :: ∀ a w. (State1 a w) -> FnObject -> EFn.EffectFn2 (KeyedState a w) (VDom a w) (VDomStep a w)
patchPartial oldPartial fnObject = EFn.mkEffectFn2 \state vdom → do
  let { build, node } = state
      (State2 oldSt _) = (unsafeCoerce oldPartial)
  case vdom of
    (PartialLayout x) ->
                let (State2 st _) = (unsafeCoerce x)
                in if not $ Util.isStateChanged oldSt st
                                then runEffectFn3 patchPartialEvaluated (unsafeCoerce x) fnObject state
                                else pure $ mkStep $ Step node state (patchPartial (unsafeCoerce oldPartial) fnObject) (haltKeyed fnObject)
    _ -> runEffectFn1 build vdom

buildMicroapp ∷ ∀ a w. VDomBuilder5 String a a w (Maybe (Array (VDom a w)))
buildMicroapp = EFn.mkEffectFn5 \(VDomSpec spec) build s as1 ch → do
  -- GET ID, SCHEDULE AN AFTER RENDER CALL TO M-APP
  -- MAYBE ADD A FUNCTION FROM PRESTO_DOM TO SCHEDULE
  requestId <- Util.generateUUID
  el ← EFn.runEffectFn3 Util.createMicroapp spec.fnObject requestId s
  let node = DOMElement.toNode el
  attrs ← EFn.runEffectFn1 (spec.buildAttributes spec.fnObject el) as1
  let onChild = EFn.mkEffectFn2 \ix child → do
                res ← EFn.runEffectFn1 build child
                EFn.runEffectFn6 Util.insertChildIx spec.fnObject "render" ix (extract res) node ""
                pure res
  children ← EFn.runEffectFn2 Util.forE (fromMaybe [] ch) onChild
  let state = { build, node, service: s, attrs, requestId : requestId, payload : Nothing, children }
  pure $ mkStep $ Step node state (patchMicroapp spec.fnObject) (haltMicroapp spec.fnObject)

patchMicroapp ∷ ∀ a w. FnObject -> EFn.EffectFn2 (MicroAppState a w) (VDom a w) (VDomStep a w)
patchMicroapp fnObject = EFn.mkEffectFn2 \state vdom → do
  let { build, node, attrs, service: value1, children : ch1} = state
  case vdom of
    Grafted g →
      EFn.runEffectFn2 (patchMicroapp fnObject) state (runGraft g)
    Microapp s2 value2 ch2 -- CHANGE IN PAYLOAD, NEEDS TO TERMINATE OLD / FIRE EVENT TO OTHER M_APP
      | value1 == s2 → do
          let
            onThese = EFn.mkEffectFn4 \obj ix s v → do
              res ← EFn.runEffectFn2 step s v
              EFn.runEffectFn6 Util.insertChildIx obj "patch" ix (extract res) node ""
              pure res
            onThis = EFn.mkEffectFn3 \_ _ s → EFn.runEffectFn1 halt s
            onThat = EFn.mkEffectFn3 \obj ix v → do
              res ← EFn.runEffectFn1 build v
              EFn.runEffectFn6 Util.insertChildIx obj "patch" ix (extract res) node ""
              pure res
          children2 ← EFn.runEffectFn6 Util.diffWithIxE fnObject ch1 (fromMaybe [] ch2) onThese onThis onThat
          attrs2 ← EFn.runEffectFn2 step attrs value2
          pure $ mkStep $ Step node (state {attrs = attrs2, children= children2}) (patchMicroapp fnObject) (haltMicroapp fnObject)
      | otherwise → do
          -- NOT HANDLED THIS IS DUMMY CODE
          -- CASE WHERE SERVICE CHANGES IS NOT ACCEPTABLE [FOR NOW]
          -- DOING NOTHING
          pure $ mkStep $ Step node state (patchMicroapp fnObject) (haltMicroapp fnObject)
    _ → do
      EFn.runEffectFn1 (haltMicroapp fnObject) state
      EFn.runEffectFn1 build vdom

haltMicroapp ∷ ∀ a w. FnObject -> EFn.EffectFn1 (MicroAppState a w) Unit
haltMicroapp fnObject = EFn.mkEffectFn1 \{ node, attrs, children } → do
  parent ← EFn.runEffectFn1 Util.parentNode node
  EFn.runEffectFn3 Util.removeChild fnObject node parent
  EFn.runEffectFn2 Util.forEachE children halt
  EFn.runEffectFn1 halt attrs

type TextState a w =
  { build ∷ VDomMachine a w
  , node ∷ DOM.Node
  , value ∷ String
  }

buildText ∷ ∀ a w. VDomBuilder String a w
buildText = EFn.mkEffectFn3 \(VDomSpec spec) build s → do
  node ← EFn.runEffectFn1 Util.createTextNode s
  let state = { build, node, value: s }
  pure $ mkStep $ Step node state (patchText spec.fnObject) (haltText spec.fnObject)

patchText ∷ ∀ a w. FnObject -> EFn.EffectFn2 (TextState a w) (VDom a w) (VDomStep a w)
patchText fnObject = EFn.mkEffectFn2 \state vdom → do
  let { build, node, value: value1 } = state
  case vdom of
    Grafted g →
      EFn.runEffectFn2 (patchText fnObject) state (runGraft g)
    Text value2
      | value1 == value2 →
          pure $ mkStep $ Step node state (patchText fnObject) (haltText fnObject)
      | otherwise → do
          let nextState = { build, node, value: value2 }
          EFn.runEffectFn2 Util.setTextContent value2 node
          pure $ mkStep $ Step node nextState (patchText fnObject) (haltText fnObject)
    _ → do
      EFn.runEffectFn1 (haltText fnObject) state
      EFn.runEffectFn1 build vdom

haltText ∷ ∀ a w. FnObject -> EFn.EffectFn1 (TextState a w) Unit
haltText fnObject = EFn.mkEffectFn1 \{ node } → do
  parent ← EFn.runEffectFn1 Util.parentNode node
  EFn.runEffectFn3 Util.removeChild fnObject node parent

type KeyedState a w =
  { build ∷ VDomMachine a w
  , node ∷ DOM.Node
  , attrs ∷ Step a Unit
  , ns ∷ Maybe Namespace
  , name ∷ ElemName
  , children ∷ Object.Object (VDomStep a w)
  , length ∷ Int
  }

buildElem ∷ ∀ a w. VDomBuilder4 (Maybe Namespace) ElemName a (Array (VDom a w)) a w
buildElem = EFn.mkEffectFn6 \spec build ns1 name1 as1 ch1 → do
  EFn.runEffectFn6 buildKeyed spec build ns1 name1 as1 (Array.mapWithIndex (\i a -> Tuple (show i) a) ch1)

isVisibilityGone :: forall a. Array (Prop a) -> Boolean
isVisibilityGone = Array.any ( case _ of
    Property "visibility" x -> unsafeCoerce x == "gone"
    _ -> false
  )

filterGoneNodes :: forall a w. Array (Tuple String (VDom a w)) -> Array (Tuple String (VDom a w))
filterGoneNodes = Array.filter ( not <<< case _ of
    Tuple _ (Elem _ _ props _) -> isVisibilityGone (unsafeCoerce props)
    Tuple _ (Keyed _ _ props _) -> isVisibilityGone (unsafeCoerce props)
    _ -> false
  )

buildKeyed ∷ ∀ a w. VDomBuilder4 (Maybe Namespace) ElemName a (Array (Tuple String (VDom a w))) a w
buildKeyed = EFn.mkEffectFn6 \(VDomSpec spec) build ns1 name1 as1 ch1 → do
  el ← EFn.runEffectFn4 Util.createElement spec.fnObject (toNullable ns1) name1 "keyed"
  let
    node = DOMElement.toNode el
    onChild = EFn.mkEffectFn4 \k ix _ (Tuple _ vdom) → do
      res ← EFn.runEffectFn1 build vdom
      EFn.runEffectFn6 Util.insertChildIx spec.fnObject "render" ix (extract res) node k
      pure res

  -- Visibility Check logic
    -- loop on children to find which nodes to eliminate
    -- eliminate nodes where there is property visibility with value gone
  let ch2 = filterGoneNodes ch1

  children ← EFn.runEffectFn3 Util.strMapWithIxE ch2 fst onChild
  attrs ← EFn.runEffectFn1 (spec.buildAttributes spec.fnObject el) as1
  let
    state =
      { build
      , node
      , attrs
      , ns: ns1
      , name: name1
      , children
      , length: Array.length ch2
      }
  pure $ mkStep $ Step node state (patchKeyed spec.fnObject) (haltKeyed spec.fnObject)

patchKeyed ∷ ∀ a w. FnObject -> EFn.EffectFn2 (KeyedState a w) (VDom a w) (VDomStep a w)
patchKeyed fnObject = EFn.mkEffectFn2 \state vdom → do
  let { build, node, attrs, ns: ns1, name: name1, children: ch1, length: len1 } = state
  let patchFunc = EFn.mkEffectFn4 \ns2 name2 as2 ch0 ->  do
        let ch2 = filterGoneNodes ch0
        case len1, Array.length ch2 of
          0, 0 → do
            attrs2 ← EFn.runEffectFn2 Machine.step attrs as2
            let
              nextState =
                { build
                , node
                , attrs: attrs2
                , ns: ns2
                , name: name2
                , children: ch1
                , length: 0
                }
            pure $ mkStep $ Step node nextState (patchKeyed fnObject) (haltKeyed fnObject)
          _, len2 → do
            let
              onThese = EFn.mkEffectFn5 \obj k ix' s (Tuple _ v) → do
                res ← EFn.runEffectFn2 step s v
                EFn.runEffectFn6 Util.insertChildIx obj "patch" ix' (extract res) node k
                pure res
              onThis = EFn.mkEffectFn3 \_ _ s → EFn.runEffectFn1 halt s
              onThat = EFn.mkEffectFn4 \obj k ix (Tuple _ v) → do
                res ← EFn.runEffectFn1 build v
                EFn.runEffectFn6 Util.insertChildIx obj "patch" ix (extract res) node k
                pure res
            children2 ← EFn.runEffectFn7 Util.diffWithKeyAndIxE fnObject ch1 ch2 fst onThese onThis onThat
            attrs2 ← EFn.runEffectFn2 step attrs as2
            let
              nextState =
                { build
                , node
                , attrs: attrs2
                , ns: ns2
                , name: name2
                , children: children2
                , length: len2
                }
            pure $ mkStep $ Step node nextState (patchKeyed fnObject) (haltKeyed fnObject)

  case vdom of
    Grafted g →
      EFn.runEffectFn2 (patchKeyed fnObject) state (runGraft g)
    Elem ns2 name2 as2 ch3 | Fn.runFn4 eqElemSpec ns1 name1 ns2 name2 →
      EFn.runEffectFn4 patchFunc ns2 name2 as2 (Array.mapWithIndex (\i a -> Tuple (show i) a) ch3)
    Keyed ns2 name2 as2 ch2 | Fn.runFn4 eqElemSpec ns1 name1 ns2 name2 →
      EFn.runEffectFn4 patchFunc ns2 name2 as2 ch2
    _ → do
      EFn.runEffectFn1 (haltKeyed fnObject) state
      EFn.runEffectFn1 build vdom

haltKeyed ∷ ∀ a w. FnObject -> EFn.EffectFn1 (KeyedState a w) Unit
haltKeyed fnObject = EFn.mkEffectFn1 \{ node, attrs, children } → do
  parent ← EFn.runEffectFn1 Util.parentNode node
  EFn.runEffectFn3 Util.removeChild fnObject node parent
  EFn.runEffectFn2 Util.forInE children (EFn.mkEffectFn2 \_ s → EFn.runEffectFn1 halt s)
  EFn.runEffectFn1 halt attrs

type WidgetState a w =
  { build ∷ VDomMachine a w
  , widget ∷ Step w DOM.Node
  }

buildWidget ∷ ∀ a w. VDomBuilder w a w
buildWidget = EFn.mkEffectFn3 \(VDomSpec spec) build w → do
  res ← EFn.runEffectFn1 (spec.buildWidget (VDomSpec spec)) w
  let
    res' = res # unStep \(Step n _ _ _) →
      mkStep $ Step n { build, widget: res } patchWidget haltWidget
  pure res'

patchWidget ∷ ∀ a w. EFn.EffectFn2 (WidgetState a w) (VDom a w) (VDomStep a w)
patchWidget = EFn.mkEffectFn2 \state vdom → do
  let { build, widget } = state
  case vdom of
    Grafted g →
      EFn.runEffectFn2 patchWidget state (runGraft g)
    Widget w → do
      res ← EFn.runEffectFn2 step widget w
      let
        res' = res # unStep \(Step n _ _ _) →
          mkStep $ Step n { build, widget: res } patchWidget haltWidget
      pure res'
    _ → do
      EFn.runEffectFn1 haltWidget state
      EFn.runEffectFn1 build vdom

haltWidget ∷ forall a w. EFn.EffectFn1 (WidgetState a w) Unit
haltWidget = EFn.mkEffectFn1 \{ widget } → do
  EFn.runEffectFn1 halt widget

type ChunkState a w =
  { build ∷ VDomMachine a w
  , node ∷ DOM.Node
  , attrs ∷ Step a Unit
  , ns ∷ Maybe Namespace
  , name ∷ ElemName
  , children ∷ Array ({ shimmer :: VDomStep a w, layout ::  VDomStep a w })
  }

buildChunk ∷ ∀ a w. VDomBuilder4 (Maybe Namespace) ElemName a (ShimmerHolder a w) a w
buildChunk = EFn.mkEffectFn6 \(VDomSpec spec) build ns1 name1 as1 ch1 → do
  el ← EFn.runEffectFn3 Util.createChunkedElement spec.fnObject (toNullable ns1) name1
  let
    node = DOMElement.toNode el
    onChild = EFn.mkEffectFn2 \ix child → do
              res1 ← EFn.runEffectFn1 build child.shimmer
              res2 ← EFn.runEffectFn1 build child.actualLayout
              let res = { shimmer: (extract res1), layout: (extract res2)}
              EFn.runEffectFn5 Util.insertChunkIx spec.fnObject "render" ix res node
              pure { shimmer: res1, layout: res2 }
  children ← EFn.runEffectFn2 Util.forE ch1 onChild
  attrs ← EFn.runEffectFn1 (spec.buildAttributes spec.fnObject el) as1
  let
    state =
      { build
      , node
      , attrs
      , ns: ns1
      , name: name1
      , children
      }
  pure $ mkStep $ Step node state (patchChunk spec.fnObject) (haltChunk spec.fnObject)

patchChunk ∷ ∀ a w. FnObject -> EFn.EffectFn2 (ChunkState a w) (VDom a w) (VDomStep a w)
patchChunk fnObject = EFn.mkEffectFn2 \state vdom → do
  let { build, node, attrs, ns: ns1, name: name1, children: ch1 } = state
  case vdom of
    Grafted g →
      EFn.runEffectFn2 (patchChunk fnObject) state (runGraft g)
    Chunk ns2 name2 as2 ch2 | Fn.runFn4 eqElemSpec ns1 name1 ns2 name2 → do
      case Array.length ch1, Array.length ch2 of
        0, 0 → do
          attrs2 ← EFn.runEffectFn2 step attrs as2
          let
            nextState =
              { build
              , node
              , attrs: attrs2
              , ns: ns2
              , name: name2
              , children: ch1
              }
          pure $ mkStep $ Step node nextState (patchChunk fnObject) (haltChunk fnObject)
        _, _ → do
          let
            onThese = EFn.mkEffectFn4 \obj ix s v → do
              -- res1 ← EFn.runEffectFn2 step s v.shimmer
              res2 ← EFn.runEffectFn2 step s v.actualLayout
              let res = { shimmer: (extract s), layout: (extract res2) }
              EFn.runEffectFn5 Util.insertChunkIx obj "patch" ix res node
              pure { shimmer: s, layout: res2 }
            onThis = EFn.mkEffectFn3 \_ _ s → EFn.runEffectFn1 halt s
            onThat = EFn.mkEffectFn3 \obj ix v → do
              res1 ← EFn.runEffectFn1 build v.shimmer
              res2 ← EFn.runEffectFn1 build v.actualLayout
              let res = { shimmer: (extract res1), layout: (extract res2)}
              EFn.runEffectFn5 Util.insertChunkIx obj "patch" ix res node
              pure { shimmer: res1, layout: res2 }
          children2 ← EFn.runEffectFn6 Util.diffChunkWithIxE fnObject ch1 ch2 onThese onThis onThat
          attrs2 ← EFn.runEffectFn2 step attrs as2
          let
            nextState =
              { build
              , node
              , attrs: attrs2
              , ns: ns2
              , name: name2
              , children: children2
              }
          pure $ mkStep $ Step node nextState (patchChunk fnObject) (haltChunk fnObject)
    _ → do
      EFn.runEffectFn1 (haltChunk fnObject) state
      EFn.runEffectFn1 build vdom

haltChunk ∷ ∀ a w. FnObject -> EFn.EffectFn1 (ChunkState a w) Unit
haltChunk _ = EFn.mkEffectFn1 \_ → do
  pure unit

eqElemSpec ∷ Fn.Fn4 (Maybe Namespace) ElemName (Maybe Namespace) ElemName Boolean
eqElemSpec = Fn.mkFn4 \ns1 (ElemName name1) ns2 (ElemName name2) →
  if name1 == name2
    then case ns1, ns2 of
      Just (Namespace ns1'), Just (Namespace ns2') | ns1' == ns2' → true
      Nothing, Nothing → true
      _, _ → false
    else false
