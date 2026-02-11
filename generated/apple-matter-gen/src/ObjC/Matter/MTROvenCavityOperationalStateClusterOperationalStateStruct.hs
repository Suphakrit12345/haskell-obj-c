{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROvenCavityOperationalStateClusterOperationalStateStruct@.
module ObjC.Matter.MTROvenCavityOperationalStateClusterOperationalStateStruct
  ( MTROvenCavityOperationalStateClusterOperationalStateStruct
  , IsMTROvenCavityOperationalStateClusterOperationalStateStruct(..)
  , operationalStateID
  , setOperationalStateID
  , operationalStateLabel
  , setOperationalStateLabel
  , operationalStateIDSelector
  , setOperationalStateIDSelector
  , operationalStateLabelSelector
  , setOperationalStateLabelSelector


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

-- | @- operationalStateID@
operationalStateID :: IsMTROvenCavityOperationalStateClusterOperationalStateStruct mtrOvenCavityOperationalStateClusterOperationalStateStruct => mtrOvenCavityOperationalStateClusterOperationalStateStruct -> IO (Id NSNumber)
operationalStateID mtrOvenCavityOperationalStateClusterOperationalStateStruct  =
    sendMsg mtrOvenCavityOperationalStateClusterOperationalStateStruct (mkSelector "operationalStateID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperationalStateID:@
setOperationalStateID :: (IsMTROvenCavityOperationalStateClusterOperationalStateStruct mtrOvenCavityOperationalStateClusterOperationalStateStruct, IsNSNumber value) => mtrOvenCavityOperationalStateClusterOperationalStateStruct -> value -> IO ()
setOperationalStateID mtrOvenCavityOperationalStateClusterOperationalStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOvenCavityOperationalStateClusterOperationalStateStruct (mkSelector "setOperationalStateID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- operationalStateLabel@
operationalStateLabel :: IsMTROvenCavityOperationalStateClusterOperationalStateStruct mtrOvenCavityOperationalStateClusterOperationalStateStruct => mtrOvenCavityOperationalStateClusterOperationalStateStruct -> IO (Id NSString)
operationalStateLabel mtrOvenCavityOperationalStateClusterOperationalStateStruct  =
    sendMsg mtrOvenCavityOperationalStateClusterOperationalStateStruct (mkSelector "operationalStateLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperationalStateLabel:@
setOperationalStateLabel :: (IsMTROvenCavityOperationalStateClusterOperationalStateStruct mtrOvenCavityOperationalStateClusterOperationalStateStruct, IsNSString value) => mtrOvenCavityOperationalStateClusterOperationalStateStruct -> value -> IO ()
setOperationalStateLabel mtrOvenCavityOperationalStateClusterOperationalStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOvenCavityOperationalStateClusterOperationalStateStruct (mkSelector "setOperationalStateLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @operationalStateID@
operationalStateIDSelector :: Selector
operationalStateIDSelector = mkSelector "operationalStateID"

-- | @Selector@ for @setOperationalStateID:@
setOperationalStateIDSelector :: Selector
setOperationalStateIDSelector = mkSelector "setOperationalStateID:"

-- | @Selector@ for @operationalStateLabel@
operationalStateLabelSelector :: Selector
operationalStateLabelSelector = mkSelector "operationalStateLabel"

-- | @Selector@ for @setOperationalStateLabel:@
setOperationalStateLabelSelector :: Selector
setOperationalStateLabelSelector = mkSelector "setOperationalStateLabel:"

