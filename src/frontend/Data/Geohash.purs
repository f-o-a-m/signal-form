module Data.Geohash
  ( Geohash(..)
  , geohashToHex
  , geohashToBS
  , geohashFromHex
  , geohashFromBS
  , geohashFromString
  ) where

import Prelude

import Data.Argonaut as A
import Data.ByteString as BS
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Int (hexadecimal, radix)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Network.Ethereum.Core.BigNumber (parseBigNumber, toString)
import Network.Ethereum.Core.HexString (HexString, mkHexString, toHexString, unHex)
import Node.Encoding (Encoding(Hex))
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

newtype Geohash = Geohash String

derive newtype instance geohashEq :: Eq Geohash
derive newtype instance geohashOrd :: Ord Geohash
derive instance geohashNewtype :: Newtype Geohash _

instance geohashShow :: Show Geohash where
  show (Geohash gh) = gh

instance decodeJsonGeohash :: A.DecodeJson Geohash where
  decodeJson = map Geohash <<< A.decodeJson

instance encodeJsonGeohash :: A.EncodeJson Geohash where
  encodeJson (Geohash gh) = A.encodeJson gh

geohashToHex :: Geohash -> HexString
geohashToHex (Geohash gh) = unsafePartial $ fromJust do
  base32 <- radix 32
  toHexString <$> parseBigNumber base32 (geoToBase32Alph gh)

geohashToBS :: Geohash -> BS.ByteString
geohashToBS gh =
  let hx = unHex <<< geohashToHex $ gh
  in unsafePartial $ fromJust $ BS.fromString hx Hex

geohashFromHex :: HexString -> Geohash
geohashFromHex hx = unsafePartial $ fromJust do
  n <- parseBigNumber hexadecimal $ unHex hx
  base32 <- radix 32
  pure <<< Geohash <<< base32AlphToGeo $ toString base32 n

geohashFromBS :: BS.ByteString -> Geohash
geohashFromBS bs = geohashFromHex <<< unsafePartial fromJust <<< mkHexString $ BS.toString bs Hex

geohashFromString :: String -> Maybe Geohash
geohashFromString n =
  case radix 32 >>= \base32 -> parseBigNumber base32 $ geoToBase32Alph n of
    Nothing -> Nothing
    _ -> Just $ Geohash n

base32AlphToGeo :: String -> String
base32AlphToGeo = fromCharArray <<< map base32ToGeoChar <<< toCharArray
  where
    base32ToGeoChar c = unsafePartial $ case c of
      '0' -> '0'
      '1' -> '1'
      '2' -> '2'
      '3' -> '3'
      '4' -> '4'
      '5' -> '5'
      '6' -> '6'
      '7' -> '7'
      '8' -> '8'
      '9' -> '9'
      'a' -> 'b'
      'b' -> 'c'
      'c' -> 'd'
      'd' -> 'e'
      'e' -> 'f'
      'f' -> 'g'
      'g' -> 'h'
      'h' -> 'j'
      'i' -> 'k'
      'j' -> 'm'
      'k' -> 'n'
      'l' -> 'p'
      'm' -> 'q'
      'n' -> 'r'
      'o' -> 's'
      'p' -> 't'
      'q' -> 'u'
      'r' -> 'v'
      's' -> 'w'
      't' -> 'x'
      'u' -> 'y'
      'v' -> 'z'

geoToBase32Alph :: String -> String
geoToBase32Alph = fromCharArray <<< map geoCharToBase32 <<< toCharArray
  where
    geoCharToBase32 c = unsafePartial $ case c of
      '0' -> '0'
      '1' -> '1'
      '2' -> '2'
      '3' -> '3'
      '4' -> '4'
      '5' -> '5'
      '6' -> '6'
      '7' -> '7'
      '8' -> '8'
      '9' -> '9'
      'b' -> 'a'
      'c' -> 'b'
      'd' -> 'c'
      'e' -> 'd'
      'f' -> 'e'
      'g' -> 'f'
      'h' -> 'g'
      'j' -> 'h'
      'k' -> 'i'
      'm' -> 'j'
      'n' -> 'k'
      'p' -> 'l'
      'q' -> 'm'
      'r' -> 'n'
      's' -> 'o'
      't' -> 'p'
      'u' -> 'q'
      'v' -> 'r'
      'w' -> 's'
      'x' -> 't'
      'y' -> 'u'
      'z' -> 'v'