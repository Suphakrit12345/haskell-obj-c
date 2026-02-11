{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBasicClusterCapabilityMinimaStruct@.
module ObjC.Matter.MTRBasicClusterCapabilityMinimaStruct
  ( MTRBasicClusterCapabilityMinimaStruct
  , IsMTRBasicClusterCapabilityMinimaStruct(..)
  , caseSessionsPerFabric
  , setCaseSessionsPerFabric
  , subscriptionsPerFabric
  , setSubscriptionsPerFabric
  , caseSessionsPerFabricSelector
  , setCaseSessionsPerFabricSelector
  , subscriptionsPerFabricSelector
  , setSubscriptionsPerFabricSelector


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

-- | @- caseSessionsPerFabric@
caseSessionsPerFabric :: IsMTRBasicClusterCapabilityMinimaStruct mtrBasicClusterCapabilityMinimaStruct => mtrBasicClusterCapabilityMinimaStruct -> IO (Id NSNumber)
caseSessionsPerFabric mtrBasicClusterCapabilityMinimaStruct  =
    sendMsg mtrBasicClusterCapabilityMinimaStruct (mkSelector "caseSessionsPerFabric") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCaseSessionsPerFabric:@
setCaseSessionsPerFabric :: (IsMTRBasicClusterCapabilityMinimaStruct mtrBasicClusterCapabilityMinimaStruct, IsNSNumber value) => mtrBasicClusterCapabilityMinimaStruct -> value -> IO ()
setCaseSessionsPerFabric mtrBasicClusterCapabilityMinimaStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBasicClusterCapabilityMinimaStruct (mkSelector "setCaseSessionsPerFabric:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- subscriptionsPerFabric@
subscriptionsPerFabric :: IsMTRBasicClusterCapabilityMinimaStruct mtrBasicClusterCapabilityMinimaStruct => mtrBasicClusterCapabilityMinimaStruct -> IO (Id NSNumber)
subscriptionsPerFabric mtrBasicClusterCapabilityMinimaStruct  =
    sendMsg mtrBasicClusterCapabilityMinimaStruct (mkSelector "subscriptionsPerFabric") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubscriptionsPerFabric:@
setSubscriptionsPerFabric :: (IsMTRBasicClusterCapabilityMinimaStruct mtrBasicClusterCapabilityMinimaStruct, IsNSNumber value) => mtrBasicClusterCapabilityMinimaStruct -> value -> IO ()
setSubscriptionsPerFabric mtrBasicClusterCapabilityMinimaStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBasicClusterCapabilityMinimaStruct (mkSelector "setSubscriptionsPerFabric:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @caseSessionsPerFabric@
caseSessionsPerFabricSelector :: Selector
caseSessionsPerFabricSelector = mkSelector "caseSessionsPerFabric"

-- | @Selector@ for @setCaseSessionsPerFabric:@
setCaseSessionsPerFabricSelector :: Selector
setCaseSessionsPerFabricSelector = mkSelector "setCaseSessionsPerFabric:"

-- | @Selector@ for @subscriptionsPerFabric@
subscriptionsPerFabricSelector :: Selector
subscriptionsPerFabricSelector = mkSelector "subscriptionsPerFabric"

-- | @Selector@ for @setSubscriptionsPerFabric:@
setSubscriptionsPerFabricSelector :: Selector
setSubscriptionsPerFabricSelector = mkSelector "setSubscriptionsPerFabric:"

