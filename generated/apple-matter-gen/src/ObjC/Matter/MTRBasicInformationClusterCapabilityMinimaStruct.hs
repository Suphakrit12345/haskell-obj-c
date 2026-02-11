{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBasicInformationClusterCapabilityMinimaStruct@.
module ObjC.Matter.MTRBasicInformationClusterCapabilityMinimaStruct
  ( MTRBasicInformationClusterCapabilityMinimaStruct
  , IsMTRBasicInformationClusterCapabilityMinimaStruct(..)
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
caseSessionsPerFabric :: IsMTRBasicInformationClusterCapabilityMinimaStruct mtrBasicInformationClusterCapabilityMinimaStruct => mtrBasicInformationClusterCapabilityMinimaStruct -> IO (Id NSNumber)
caseSessionsPerFabric mtrBasicInformationClusterCapabilityMinimaStruct  =
    sendMsg mtrBasicInformationClusterCapabilityMinimaStruct (mkSelector "caseSessionsPerFabric") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCaseSessionsPerFabric:@
setCaseSessionsPerFabric :: (IsMTRBasicInformationClusterCapabilityMinimaStruct mtrBasicInformationClusterCapabilityMinimaStruct, IsNSNumber value) => mtrBasicInformationClusterCapabilityMinimaStruct -> value -> IO ()
setCaseSessionsPerFabric mtrBasicInformationClusterCapabilityMinimaStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBasicInformationClusterCapabilityMinimaStruct (mkSelector "setCaseSessionsPerFabric:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- subscriptionsPerFabric@
subscriptionsPerFabric :: IsMTRBasicInformationClusterCapabilityMinimaStruct mtrBasicInformationClusterCapabilityMinimaStruct => mtrBasicInformationClusterCapabilityMinimaStruct -> IO (Id NSNumber)
subscriptionsPerFabric mtrBasicInformationClusterCapabilityMinimaStruct  =
    sendMsg mtrBasicInformationClusterCapabilityMinimaStruct (mkSelector "subscriptionsPerFabric") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubscriptionsPerFabric:@
setSubscriptionsPerFabric :: (IsMTRBasicInformationClusterCapabilityMinimaStruct mtrBasicInformationClusterCapabilityMinimaStruct, IsNSNumber value) => mtrBasicInformationClusterCapabilityMinimaStruct -> value -> IO ()
setSubscriptionsPerFabric mtrBasicInformationClusterCapabilityMinimaStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBasicInformationClusterCapabilityMinimaStruct (mkSelector "setSubscriptionsPerFabric:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

