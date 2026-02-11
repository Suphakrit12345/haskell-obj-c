{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCOperationalStateClusterOperationalStateStruct@.
module ObjC.Matter.MTRRVCOperationalStateClusterOperationalStateStruct
  ( MTRRVCOperationalStateClusterOperationalStateStruct
  , IsMTRRVCOperationalStateClusterOperationalStateStruct(..)
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
operationalStateID :: IsMTRRVCOperationalStateClusterOperationalStateStruct mtrrvcOperationalStateClusterOperationalStateStruct => mtrrvcOperationalStateClusterOperationalStateStruct -> IO (Id NSNumber)
operationalStateID mtrrvcOperationalStateClusterOperationalStateStruct  =
    sendMsg mtrrvcOperationalStateClusterOperationalStateStruct (mkSelector "operationalStateID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperationalStateID:@
setOperationalStateID :: (IsMTRRVCOperationalStateClusterOperationalStateStruct mtrrvcOperationalStateClusterOperationalStateStruct, IsNSNumber value) => mtrrvcOperationalStateClusterOperationalStateStruct -> value -> IO ()
setOperationalStateID mtrrvcOperationalStateClusterOperationalStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcOperationalStateClusterOperationalStateStruct (mkSelector "setOperationalStateID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- operationalStateLabel@
operationalStateLabel :: IsMTRRVCOperationalStateClusterOperationalStateStruct mtrrvcOperationalStateClusterOperationalStateStruct => mtrrvcOperationalStateClusterOperationalStateStruct -> IO (Id NSString)
operationalStateLabel mtrrvcOperationalStateClusterOperationalStateStruct  =
    sendMsg mtrrvcOperationalStateClusterOperationalStateStruct (mkSelector "operationalStateLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOperationalStateLabel:@
setOperationalStateLabel :: (IsMTRRVCOperationalStateClusterOperationalStateStruct mtrrvcOperationalStateClusterOperationalStateStruct, IsNSString value) => mtrrvcOperationalStateClusterOperationalStateStruct -> value -> IO ()
setOperationalStateLabel mtrrvcOperationalStateClusterOperationalStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcOperationalStateClusterOperationalStateStruct (mkSelector "setOperationalStateLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

