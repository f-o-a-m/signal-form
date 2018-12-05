module App.Form.Component where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Effect.Console (logShow)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


import Network.Ethereum.Core.HexString (HexString)
import Data.Geohash (Geohash, geohashFromString, geohashToHex)
import Formless.Validation (hoistFnE_)


--------------------------------------------------------------------------------
-- Component
--------------------------------------------------------------------------------



data Query a = Formless (F.Message' SignalForm) a

type ChildQuery = F.Query' SignalForm Aff
type ChildSlot = Unit

data FieldError =
    InvalidGeohash

component :: H.Component HH.HTML Query Unit Void Aff
component = H.parentComponent
  { initialState: const unit
  , render
  , eval
  , receiver: const Nothing
  }
  where

  render :: Unit -> H.ParentHTML Query ChildQuery ChildSlot Aff
  render st =
    HH.section_
    [ HH.h1_ [ HH.text "Geohash Converter" ]
    , HH.h2_ [ HH.text "Convert a base-32 geohash into a base-16 bytestring." ]
    , HH.br_
    , HH.slot unit F.component { initialInputs, validators, render: renderFormless } (HE.input Formless)
    ]

  eval :: Query ~> H.ParentDSL Unit Query ChildQuery ChildSlot Void Aff
  eval (Formless (F.Submitted formOutputs) a) = a <$ do
    -- To unwrap the OutputField newtypes on each field and the overall ContactForm newtype,
    -- use the unwrapOutputFields helper.
    let contact :: Signal
        contact = F.unwrapOutputFields formOutputs
    H.liftEffect $ logShow contact

  -- In this example we can ignore other outputs, but see the other examples for more
  -- in-depth usage.
  eval (Formless _ a) = pure a

--------------------------------------------------------------------------------
-- Formless
--------------------------------------------------------------------------------



type Signal = { geohashAsHex :: HexString }

newtype SignalForm r f = SignalForm (r
  ( geohashAsHex :: FieldError String HexString
  ))
derive instance newtypeSignalForm :: Newtype (SignalForm r f) _

initialInputs :: SignalForm Record F.InputField
initialInputs = F.wrapInputFields { geohash: "" }

validators :: SignalForm Record (F.Validation SignalForm Aff)
validators = SignalForm { geohash: hoistFnE_ $ \gh ->
                            case geohashFromString gh of
                              Nothing -> Left InvalidGeohash
                              Right gh -> Right $ geohashToHex gh
                        }

renderFormless :: F.State SignalForm Aff -> F.HTML' SignalForm Aff
renderFormless state =
 HH.div_
 [ HH.input
     [ HP.value $ F.getInput _geohash state.form
     , HE.onValueInput $ HE.input $ F.setValidate _geohash
     ]
   , HH.button
     [ HE.onClick $ HE.input_ F.submit ]
     [ HH.text "Convert" ]
   ]
  where
    _geohash = SProxy :: SProxy "geohash"
