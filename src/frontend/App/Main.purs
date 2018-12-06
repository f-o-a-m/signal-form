module App.Main where

import Prelude

import App.Form.Component as F
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as C
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafeCrashWith)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = C.log "hello" *> HA.runHalogenAff do
    mappElem <- HA.selectElement $ QuerySelector "#app"
    case mappElem of
      Nothing -> unsafeCrashWith "div#app has to be defined"
      Just appElem -> do
        runUI F.component F.initialState appElem
