{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains an amount of money that the data detection system matches.
--
-- The DataDetection framework returns a match for an amount of money in a @DDMatchMoneyAmount@ object, which contains an amount of money and an ISO currency code.
--
-- Generated bindings for @DDMatchMoneyAmount@.
module ObjC.DataDetection.DDMatchMoneyAmount
  ( DDMatchMoneyAmount
  , IsDDMatchMoneyAmount(..)
  , currency
  , amount
  , currencySelector
  , amountSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.DataDetection.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A string that contains an ISO currency code, which the data detection system identifies from the matched string and user preferences.
--
-- ObjC selector: @- currency@
currency :: IsDDMatchMoneyAmount ddMatchMoneyAmount => ddMatchMoneyAmount -> IO (Id NSString)
currency ddMatchMoneyAmount  =
    sendMsg ddMatchMoneyAmount (mkSelector "currency") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A number that represents an amount of money.
--
-- ObjC selector: @- amount@
amount :: IsDDMatchMoneyAmount ddMatchMoneyAmount => ddMatchMoneyAmount -> IO CDouble
amount ddMatchMoneyAmount  =
    sendMsg ddMatchMoneyAmount (mkSelector "amount") retCDouble []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currency@
currencySelector :: Selector
currencySelector = mkSelector "currency"

-- | @Selector@ for @amount@
amountSelector :: Selector
amountSelector = mkSelector "amount"

