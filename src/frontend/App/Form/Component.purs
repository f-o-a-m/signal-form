module App.Form.Component where

import Prelude

import Data.Array (elem, foldMap)
import Data.Either (Either(..))
import Data.Geohash (geohashFromString, geohashToHex)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Conj (Conj(..))
import Data.Newtype (class Newtype, un)
import Data.String (null)
import Data.String.CodeUnits (toCharArray)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Formless as F
import Formless.Validation (hoistFnE_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.Ethereum.Core.HexString (HexString)

--------------------------------------------------------------------------------
-- Component
--------------------------------------------------------------------------------

data Query a = Formless (F.Message' SignalForm) a

type ChildQuery = F.Query' SignalForm Aff
type ChildSlot = Unit

data FieldError =
    Required
  | InvalidGeohash

type State = {geohash :: Maybe HexString, errorMsg :: Maybe String}

initialState :: State
initialState = {geohash: Nothing, errorMsg: Nothing}

component :: H.Component HH.HTML Query State Void Aff
component = H.parentComponent
  { initialState: identity
  , render
  , eval
  , receiver: const Nothing
  }
  where

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
  render st =
    HH.section_
    [ HH.h1_ [ HH.text "Geohash Converter" ]
    , HH.p_ [ HH.text "Convert a base-32 geohash into a base-16 bytestring." ]
    , HH.br_
    , HH.slot unit F.component { initialInputs, validators, render: renderFormless } (HE.input Formless)
    , HH.div_ [HH.text $ maybe mempty show st.geohash]
    , HH.div_ [HH.text $ maybe mempty identity st.errorMsg]
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
  eval (Formless (F.Submitted formOutputs) a) = a <$ do
    let signal :: Signal
        signal = F.unwrapOutputFields formOutputs
    H.modify_ _{geohash = Just signal.geohashAsHex}

  eval (Formless (F.Changed formState) a) = a <$ do
    H.modify_ _{geohash = Nothing}
    if formState.dirty && formState.errors /= 0
       then H.modify_ _{errorMsg = Just "Invalid geohash input"}
       else H.modify_ _{errorMsg = Nothing}
  eval (Formless _ a) = pure a

--------------------------------------------------------------------------------
-- Formless
--------------------------------------------------------------------------------

type Signal = { geohashAsHex :: HexString }

newtype SignalForm r f = SignalForm (r
  ( geohashAsHex :: f FieldError String HexString
  ))
derive instance newtypeSignalForm :: Newtype (SignalForm r f) _

initialInputs :: SignalForm Record F.InputField
initialInputs = F.wrapInputFields { geohashAsHex: mempty :: String }

-- For example, this validator makes sure that the string is not empty
isNonEmpty :: ∀ form m. Monad m => F.Validation form m FieldError String String
isNonEmpty = hoistFnE_ $ \str ->
  if null str
     then Left Required
     else Right str

validators :: SignalForm Record (F.Validation SignalForm Aff)
validators = SignalForm { geohashAsHex: isNonEmpty >>> (hoistFnE_ $ \gh ->
                           if not $ allGeoChars (toCharArray gh)
                              then Left InvalidGeohash
                              else case geohashFromString gh of
                                     Nothing -> Left InvalidGeohash
                                     Just gh' -> Right $ geohashToHex gh')
                        }
  where
    allGeoChars s = un Conj $ foldMap (Conj <<< isGeoChar) s
    isGeoChar a = a `elem` ['0','1','2','3','4','5','6','7','8','9','b'
                           ,'c','d','e','f','g','h','j','k','m','n','p'
                           ,'q','r','s','t','u','v','w','x','y','z'
                           ]

renderFormless :: F.State SignalForm Aff -> F.HTML' SignalForm Aff
renderFormless state =
 HH.div [HP.classes [H.ClassName "form-inline"]]
 [ HH.input
     [ HP.value $ F.getInput _geohash state.form
     , HE.onValueInput $ HE.input $ F.setValidate _geohash
     ]
   , HH.div [HP.classes [H.ClassName "button"]]
     [ HH.button
       [ HE.onClick $ HE.input_ F.submit ]
       [ HH.text "Convert" ]
     ]
   ]
  where
    _geohash = SProxy :: SProxy "geohashAsHex"
