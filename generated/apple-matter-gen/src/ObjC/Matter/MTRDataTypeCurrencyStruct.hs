{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeCurrencyStruct@.
module ObjC.Matter.MTRDataTypeCurrencyStruct
  ( MTRDataTypeCurrencyStruct
  , IsMTRDataTypeCurrencyStruct(..)
  , currency
  , setCurrency
  , decimalPoints
  , setDecimalPoints
  , currencySelector
  , setCurrencySelector
  , decimalPointsSelector
  , setDecimalPointsSelector


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

-- | @- currency@
currency :: IsMTRDataTypeCurrencyStruct mtrDataTypeCurrencyStruct => mtrDataTypeCurrencyStruct -> IO (Id NSNumber)
currency mtrDataTypeCurrencyStruct  =
    sendMsg mtrDataTypeCurrencyStruct (mkSelector "currency") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCurrency:@
setCurrency :: (IsMTRDataTypeCurrencyStruct mtrDataTypeCurrencyStruct, IsNSNumber value) => mtrDataTypeCurrencyStruct -> value -> IO ()
setCurrency mtrDataTypeCurrencyStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeCurrencyStruct (mkSelector "setCurrency:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- decimalPoints@
decimalPoints :: IsMTRDataTypeCurrencyStruct mtrDataTypeCurrencyStruct => mtrDataTypeCurrencyStruct -> IO (Id NSNumber)
decimalPoints mtrDataTypeCurrencyStruct  =
    sendMsg mtrDataTypeCurrencyStruct (mkSelector "decimalPoints") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDecimalPoints:@
setDecimalPoints :: (IsMTRDataTypeCurrencyStruct mtrDataTypeCurrencyStruct, IsNSNumber value) => mtrDataTypeCurrencyStruct -> value -> IO ()
setDecimalPoints mtrDataTypeCurrencyStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeCurrencyStruct (mkSelector "setDecimalPoints:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @currency@
currencySelector :: Selector
currencySelector = mkSelector "currency"

-- | @Selector@ for @setCurrency:@
setCurrencySelector :: Selector
setCurrencySelector = mkSelector "setCurrency:"

-- | @Selector@ for @decimalPoints@
decimalPointsSelector :: Selector
decimalPointsSelector = mkSelector "decimalPoints"

-- | @Selector@ for @setDecimalPoints:@
setDecimalPointsSelector :: Selector
setDecimalPointsSelector = mkSelector "setDecimalPoints:"

