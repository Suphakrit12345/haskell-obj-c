{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRApplicationLauncherClusterApplicationEPStruct@.
module ObjC.Matter.MTRApplicationLauncherClusterApplicationEPStruct
  ( MTRApplicationLauncherClusterApplicationEPStruct
  , IsMTRApplicationLauncherClusterApplicationEPStruct(..)
  , application
  , setApplication
  , endpoint
  , setEndpoint
  , applicationSelector
  , setApplicationSelector
  , endpointSelector
  , setEndpointSelector


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

-- | @- application@
application :: IsMTRApplicationLauncherClusterApplicationEPStruct mtrApplicationLauncherClusterApplicationEPStruct => mtrApplicationLauncherClusterApplicationEPStruct -> IO (Id MTRApplicationLauncherClusterApplicationStruct)
application mtrApplicationLauncherClusterApplicationEPStruct  =
    sendMsg mtrApplicationLauncherClusterApplicationEPStruct (mkSelector "application") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setApplication:@
setApplication :: (IsMTRApplicationLauncherClusterApplicationEPStruct mtrApplicationLauncherClusterApplicationEPStruct, IsMTRApplicationLauncherClusterApplicationStruct value) => mtrApplicationLauncherClusterApplicationEPStruct -> value -> IO ()
setApplication mtrApplicationLauncherClusterApplicationEPStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrApplicationLauncherClusterApplicationEPStruct (mkSelector "setApplication:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpoint@
endpoint :: IsMTRApplicationLauncherClusterApplicationEPStruct mtrApplicationLauncherClusterApplicationEPStruct => mtrApplicationLauncherClusterApplicationEPStruct -> IO (Id NSNumber)
endpoint mtrApplicationLauncherClusterApplicationEPStruct  =
    sendMsg mtrApplicationLauncherClusterApplicationEPStruct (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoint:@
setEndpoint :: (IsMTRApplicationLauncherClusterApplicationEPStruct mtrApplicationLauncherClusterApplicationEPStruct, IsNSNumber value) => mtrApplicationLauncherClusterApplicationEPStruct -> value -> IO ()
setEndpoint mtrApplicationLauncherClusterApplicationEPStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrApplicationLauncherClusterApplicationEPStruct (mkSelector "setEndpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @application@
applicationSelector :: Selector
applicationSelector = mkSelector "application"

-- | @Selector@ for @setApplication:@
setApplicationSelector :: Selector
setApplicationSelector = mkSelector "setApplication:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector
setEndpointSelector = mkSelector "setEndpoint:"

