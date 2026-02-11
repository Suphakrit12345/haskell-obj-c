{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRHEPAFilterMonitoringClusterReplacementProductStruct@.
module ObjC.Matter.MTRHEPAFilterMonitoringClusterReplacementProductStruct
  ( MTRHEPAFilterMonitoringClusterReplacementProductStruct
  , IsMTRHEPAFilterMonitoringClusterReplacementProductStruct(..)
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
productIdentifierType :: IsMTRHEPAFilterMonitoringClusterReplacementProductStruct mtrhepaFilterMonitoringClusterReplacementProductStruct => mtrhepaFilterMonitoringClusterReplacementProductStruct -> IO (Id NSNumber)
productIdentifierType mtrhepaFilterMonitoringClusterReplacementProductStruct  =
    sendMsg mtrhepaFilterMonitoringClusterReplacementProductStruct (mkSelector "productIdentifierType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProductIdentifierType:@
setProductIdentifierType :: (IsMTRHEPAFilterMonitoringClusterReplacementProductStruct mtrhepaFilterMonitoringClusterReplacementProductStruct, IsNSNumber value) => mtrhepaFilterMonitoringClusterReplacementProductStruct -> value -> IO ()
setProductIdentifierType mtrhepaFilterMonitoringClusterReplacementProductStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrhepaFilterMonitoringClusterReplacementProductStruct (mkSelector "setProductIdentifierType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- productIdentifierValue@
productIdentifierValue :: IsMTRHEPAFilterMonitoringClusterReplacementProductStruct mtrhepaFilterMonitoringClusterReplacementProductStruct => mtrhepaFilterMonitoringClusterReplacementProductStruct -> IO (Id NSString)
productIdentifierValue mtrhepaFilterMonitoringClusterReplacementProductStruct  =
    sendMsg mtrhepaFilterMonitoringClusterReplacementProductStruct (mkSelector "productIdentifierValue") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProductIdentifierValue:@
setProductIdentifierValue :: (IsMTRHEPAFilterMonitoringClusterReplacementProductStruct mtrhepaFilterMonitoringClusterReplacementProductStruct, IsNSString value) => mtrhepaFilterMonitoringClusterReplacementProductStruct -> value -> IO ()
setProductIdentifierValue mtrhepaFilterMonitoringClusterReplacementProductStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrhepaFilterMonitoringClusterReplacementProductStruct (mkSelector "setProductIdentifierValue:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

