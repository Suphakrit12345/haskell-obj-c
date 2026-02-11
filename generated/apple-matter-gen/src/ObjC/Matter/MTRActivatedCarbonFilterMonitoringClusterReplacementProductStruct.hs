{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRActivatedCarbonFilterMonitoringClusterReplacementProductStruct@.
module ObjC.Matter.MTRActivatedCarbonFilterMonitoringClusterReplacementProductStruct
  ( MTRActivatedCarbonFilterMonitoringClusterReplacementProductStruct
  , IsMTRActivatedCarbonFilterMonitoringClusterReplacementProductStruct(..)
  , productIdentifierType
  , setProductIdentifierType
  , productIdentifierValue
  , setProductIdentifierValue
  , productIdentifierTypeSelector
  , setProductIdentifierTypeSelector
  , productIdentifierValueSelector
  , setProductIdentifierValueSelector


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

-- | @- productIdentifierType@
productIdentifierType :: IsMTRActivatedCarbonFilterMonitoringClusterReplacementProductStruct mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct => mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct -> IO (Id NSNumber)
productIdentifierType mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct  =
    sendMsg mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct (mkSelector "productIdentifierType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProductIdentifierType:@
setProductIdentifierType :: (IsMTRActivatedCarbonFilterMonitoringClusterReplacementProductStruct mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct, IsNSNumber value) => mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct -> value -> IO ()
setProductIdentifierType mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct (mkSelector "setProductIdentifierType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- productIdentifierValue@
productIdentifierValue :: IsMTRActivatedCarbonFilterMonitoringClusterReplacementProductStruct mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct => mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct -> IO (Id NSString)
productIdentifierValue mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct  =
    sendMsg mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct (mkSelector "productIdentifierValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProductIdentifierValue:@
setProductIdentifierValue :: (IsMTRActivatedCarbonFilterMonitoringClusterReplacementProductStruct mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct, IsNSString value) => mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct -> value -> IO ()
setProductIdentifierValue mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrActivatedCarbonFilterMonitoringClusterReplacementProductStruct (mkSelector "setProductIdentifierValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @productIdentifierType@
productIdentifierTypeSelector :: Selector
productIdentifierTypeSelector = mkSelector "productIdentifierType"

-- | @Selector@ for @setProductIdentifierType:@
setProductIdentifierTypeSelector :: Selector
setProductIdentifierTypeSelector = mkSelector "setProductIdentifierType:"

-- | @Selector@ for @productIdentifierValue@
productIdentifierValueSelector :: Selector
productIdentifierValueSelector = mkSelector "productIdentifierValue"

-- | @Selector@ for @setProductIdentifierValue:@
setProductIdentifierValueSelector :: Selector
setProductIdentifierValueSelector = mkSelector "setProductIdentifierValue:"

