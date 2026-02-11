{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRBasicClusterStartUpEvent@.
module ObjC.Matter.MTRBasicClusterStartUpEvent
  ( MTRBasicClusterStartUpEvent
  , IsMTRBasicClusterStartUpEvent(..)
  , softwareVersion
  , setSoftwareVersion
  , softwareVersionSelector
  , setSoftwareVersionSelector


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

-- | @- softwareVersion@
softwareVersion :: IsMTRBasicClusterStartUpEvent mtrBasicClusterStartUpEvent => mtrBasicClusterStartUpEvent -> IO (Id NSNumber)
softwareVersion mtrBasicClusterStartUpEvent  =
    sendMsg mtrBasicClusterStartUpEvent (mkSelector "softwareVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSoftwareVersion:@
setSoftwareVersion :: (IsMTRBasicClusterStartUpEvent mtrBasicClusterStartUpEvent, IsNSNumber value) => mtrBasicClusterStartUpEvent -> value -> IO ()
setSoftwareVersion mtrBasicClusterStartUpEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrBasicClusterStartUpEvent (mkSelector "setSoftwareVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @softwareVersion@
softwareVersionSelector :: Selector
softwareVersionSelector = mkSelector "softwareVersion"

-- | @Selector@ for @setSoftwareVersion:@
setSoftwareVersionSelector :: Selector
setSoftwareVersionSelector = mkSelector "setSoftwareVersion:"

