module Spec.Main where

import Prelude

import Data.Geohash as G
import Data.Maybe (fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [consoleReporter] do
  describe "Geohash" $ do
    let geohash = unsafePartial fromJust $ G.geohashFromString "2svknwgkebwt"
    it "computes Geohash to Hexstring and vice versa" $
      let roundtrip = G.geohashFromHex $ G.geohashToHex geohash
      in roundtrip `shouldEqual` geohash
    it "computes Geohash to geohashToBS and vice versa" $
      let roundtrip = G.geohashFromBS $ G.geohashToBS geohash
      in roundtrip `shouldEqual` geohash
