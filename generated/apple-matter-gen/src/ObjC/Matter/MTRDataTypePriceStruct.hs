{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypePriceStruct@.
module ObjC.Matter.MTRDataTypePriceStruct
  ( MTRDataTypePriceStruct
  , IsMTRDataTypePriceStruct(..)
  , amount
  , setAmount
  , currency
  , setCurrency
  , amountSelector
  , setAmountSelector
  , currencySelector
  , setCurrencySelector


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

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- amount@
amount :: IsMTRDataTypePriceStruct mtrDataTypePriceStruct => mtrDataTypePriceStruct -> IO (Id NSNumber)
amount mtrDataTypePriceStruct  =
    sendMsg mtrDataTypePriceStruct (mkSelector "amount") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAmount:@
setAmount :: (IsMTRDataTypePriceStruct mtrDataTypePriceStruct, IsNSNumber value) => mtrDataTypePriceStruct -> value -> IO ()
setAmount mtrDataTypePriceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypePriceStruct (mkSelector "setAmount:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- currency@
currency :: IsMTRDataTypePriceStruct mtrDataTypePriceStruct => mtrDataTypePriceStruct -> IO (Id MTRDataTypeCurrencyStruct)
currency mtrDataTypePriceStruct  =
    sendMsg mtrDataTypePriceStruct (mkSelector "currency") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrency:@
setCurrency :: (IsMTRDataTypePriceStruct mtrDataTypePriceStruct, IsMTRDataTypeCurrencyStruct value) => mtrDataTypePriceStruct -> value -> IO ()
setCurrency mtrDataTypePriceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypePriceStruct (mkSelector "setCurrency:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @amount@
amountSelector :: Selector
amountSelector = mkSelector "amount"

-- | @Selector@ for @setAmount:@
setAmountSelector :: Selector
setAmountSelector = mkSelector "setAmount:"

-- | @Selector@ for @currency@
currencySelector :: Selector
currencySelector = mkSelector "currency"

-- | @Selector@ for @setCurrency:@
setCurrencySelector :: Selector
setCurrencySelector = mkSelector "setCurrency:"

