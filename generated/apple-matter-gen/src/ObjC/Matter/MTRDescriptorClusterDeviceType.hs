{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDescriptorClusterDeviceType@.
module ObjC.Matter.MTRDescriptorClusterDeviceType
  ( MTRDescriptorClusterDeviceType
  , IsMTRDescriptorClusterDeviceType(..)
  , revision
  , setRevision
  , revisionSelector
  , setRevisionSelector


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

-- | @- revision@
revision :: IsMTRDescriptorClusterDeviceType mtrDescriptorClusterDeviceType => mtrDescriptorClusterDeviceType -> IO (Id NSNumber)
revision mtrDescriptorClusterDeviceType  =
    sendMsg mtrDescriptorClusterDeviceType (mkSelector "revision") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRevision:@
setRevision :: (IsMTRDescriptorClusterDeviceType mtrDescriptorClusterDeviceType, IsNSNumber value) => mtrDescriptorClusterDeviceType -> value -> IO ()
setRevision mtrDescriptorClusterDeviceType  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDescriptorClusterDeviceType (mkSelector "setRevision:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @revision@
revisionSelector :: Selector
revisionSelector = mkSelector "revision"

-- | @Selector@ for @setRevision:@
setRevisionSelector :: Selector
setRevisionSelector = mkSelector "setRevision:"

