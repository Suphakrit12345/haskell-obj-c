{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROtaSoftwareUpdateRequestorClusterVersionAppliedEvent@.
module ObjC.Matter.MTROtaSoftwareUpdateRequestorClusterVersionAppliedEvent
  ( MTROtaSoftwareUpdateRequestorClusterVersionAppliedEvent
  , IsMTROtaSoftwareUpdateRequestorClusterVersionAppliedEvent(..)
  , softwareVersion
  , setSoftwareVersion
  , productID
  , setProductID
  , softwareVersionSelector
  , setSoftwareVersionSelector
  , productIDSelector
  , setProductIDSelector


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
softwareVersion :: IsMTROtaSoftwareUpdateRequestorClusterVersionAppliedEvent mtrOtaSoftwareUpdateRequestorClusterVersionAppliedEvent => mtrOtaSoftwareUpdateRequestorClusterVersionAppliedEvent -> IO (Id NSNumber)
softwareVersion mtrOtaSoftwareUpdateRequestorClusterVersionAppliedEvent  =
    sendMsg mtrOtaSoftwareUpdateRequestorClusterVersionAppliedEvent (mkSelector "softwareVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSoftwareVersion:@
setSoftwareVersion :: (IsMTROtaSoftwareUpdateRequestorClusterVersionAppliedEvent mtrOtaSoftwareUpdateRequestorClusterVersionAppliedEvent, IsNSNumber value) => mtrOtaSoftwareUpdateRequestorClusterVersionAppliedEvent -> value -> IO ()
setSoftwareVersion mtrOtaSoftwareUpdateRequestorClusterVersionAppliedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateRequestorClusterVersionAppliedEvent (mkSelector "setSoftwareVersion:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- productID@
productID :: IsMTROtaSoftwareUpdateRequestorClusterVersionAppliedEvent mtrOtaSoftwareUpdateRequestorClusterVersionAppliedEvent => mtrOtaSoftwareUpdateRequestorClusterVersionAppliedEvent -> IO (Id NSNumber)
productID mtrOtaSoftwareUpdateRequestorClusterVersionAppliedEvent  =
    sendMsg mtrOtaSoftwareUpdateRequestorClusterVersionAppliedEvent (mkSelector "productID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setProductID:@
setProductID :: (IsMTROtaSoftwareUpdateRequestorClusterVersionAppliedEvent mtrOtaSoftwareUpdateRequestorClusterVersionAppliedEvent, IsNSNumber value) => mtrOtaSoftwareUpdateRequestorClusterVersionAppliedEvent -> value -> IO ()
setProductID mtrOtaSoftwareUpdateRequestorClusterVersionAppliedEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOtaSoftwareUpdateRequestorClusterVersionAppliedEvent (mkSelector "setProductID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @softwareVersion@
softwareVersionSelector :: Selector
softwareVersionSelector = mkSelector "softwareVersion"

-- | @Selector@ for @setSoftwareVersion:@
setSoftwareVersionSelector :: Selector
setSoftwareVersionSelector = mkSelector "setSoftwareVersion:"

-- | @Selector@ for @productID@
productIDSelector :: Selector
productIDSelector = mkSelector "productID"

-- | @Selector@ for @setProductID:@
setProductIDSelector :: Selector
setProductIDSelector = mkSelector "setProductID:"

