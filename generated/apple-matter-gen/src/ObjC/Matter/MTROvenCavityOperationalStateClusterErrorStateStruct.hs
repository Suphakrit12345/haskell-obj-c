{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROvenCavityOperationalStateClusterErrorStateStruct@.
module ObjC.Matter.MTROvenCavityOperationalStateClusterErrorStateStruct
  ( MTROvenCavityOperationalStateClusterErrorStateStruct
  , IsMTROvenCavityOperationalStateClusterErrorStateStruct(..)
  , errorStateID
  , setErrorStateID
  , errorStateLabel
  , setErrorStateLabel
  , errorStateDetails
  , setErrorStateDetails
  , errorStateIDSelector
  , setErrorStateIDSelector
  , errorStateLabelSelector
  , setErrorStateLabelSelector
  , errorStateDetailsSelector
  , setErrorStateDetailsSelector


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

-- | @- errorStateID@
errorStateID :: IsMTROvenCavityOperationalStateClusterErrorStateStruct mtrOvenCavityOperationalStateClusterErrorStateStruct => mtrOvenCavityOperationalStateClusterErrorStateStruct -> IO (Id NSNumber)
errorStateID mtrOvenCavityOperationalStateClusterErrorStateStruct  =
    sendMsg mtrOvenCavityOperationalStateClusterErrorStateStruct (mkSelector "errorStateID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setErrorStateID:@
setErrorStateID :: (IsMTROvenCavityOperationalStateClusterErrorStateStruct mtrOvenCavityOperationalStateClusterErrorStateStruct, IsNSNumber value) => mtrOvenCavityOperationalStateClusterErrorStateStruct -> value -> IO ()
setErrorStateID mtrOvenCavityOperationalStateClusterErrorStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOvenCavityOperationalStateClusterErrorStateStruct (mkSelector "setErrorStateID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- errorStateLabel@
errorStateLabel :: IsMTROvenCavityOperationalStateClusterErrorStateStruct mtrOvenCavityOperationalStateClusterErrorStateStruct => mtrOvenCavityOperationalStateClusterErrorStateStruct -> IO (Id NSString)
errorStateLabel mtrOvenCavityOperationalStateClusterErrorStateStruct  =
    sendMsg mtrOvenCavityOperationalStateClusterErrorStateStruct (mkSelector "errorStateLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setErrorStateLabel:@
setErrorStateLabel :: (IsMTROvenCavityOperationalStateClusterErrorStateStruct mtrOvenCavityOperationalStateClusterErrorStateStruct, IsNSString value) => mtrOvenCavityOperationalStateClusterErrorStateStruct -> value -> IO ()
setErrorStateLabel mtrOvenCavityOperationalStateClusterErrorStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOvenCavityOperationalStateClusterErrorStateStruct (mkSelector "setErrorStateLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- errorStateDetails@
errorStateDetails :: IsMTROvenCavityOperationalStateClusterErrorStateStruct mtrOvenCavityOperationalStateClusterErrorStateStruct => mtrOvenCavityOperationalStateClusterErrorStateStruct -> IO (Id NSString)
errorStateDetails mtrOvenCavityOperationalStateClusterErrorStateStruct  =
    sendMsg mtrOvenCavityOperationalStateClusterErrorStateStruct (mkSelector "errorStateDetails") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setErrorStateDetails:@
setErrorStateDetails :: (IsMTROvenCavityOperationalStateClusterErrorStateStruct mtrOvenCavityOperationalStateClusterErrorStateStruct, IsNSString value) => mtrOvenCavityOperationalStateClusterErrorStateStruct -> value -> IO ()
setErrorStateDetails mtrOvenCavityOperationalStateClusterErrorStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOvenCavityOperationalStateClusterErrorStateStruct (mkSelector "setErrorStateDetails:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @errorStateID@
errorStateIDSelector :: Selector
errorStateIDSelector = mkSelector "errorStateID"

-- | @Selector@ for @setErrorStateID:@
setErrorStateIDSelector :: Selector
setErrorStateIDSelector = mkSelector "setErrorStateID:"

-- | @Selector@ for @errorStateLabel@
errorStateLabelSelector :: Selector
errorStateLabelSelector = mkSelector "errorStateLabel"

-- | @Selector@ for @setErrorStateLabel:@
setErrorStateLabelSelector :: Selector
setErrorStateLabelSelector = mkSelector "setErrorStateLabel:"

-- | @Selector@ for @errorStateDetails@
errorStateDetailsSelector :: Selector
errorStateDetailsSelector = mkSelector "errorStateDetails"

-- | @Selector@ for @setErrorStateDetails:@
setErrorStateDetailsSelector :: Selector
setErrorStateDetailsSelector = mkSelector "setErrorStateDetails:"

