{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCOperationalStateClusterErrorStateStruct@.
module ObjC.Matter.MTRRVCOperationalStateClusterErrorStateStruct
  ( MTRRVCOperationalStateClusterErrorStateStruct
  , IsMTRRVCOperationalStateClusterErrorStateStruct(..)
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
errorStateID :: IsMTRRVCOperationalStateClusterErrorStateStruct mtrrvcOperationalStateClusterErrorStateStruct => mtrrvcOperationalStateClusterErrorStateStruct -> IO (Id NSNumber)
errorStateID mtrrvcOperationalStateClusterErrorStateStruct  =
    sendMsg mtrrvcOperationalStateClusterErrorStateStruct (mkSelector "errorStateID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setErrorStateID:@
setErrorStateID :: (IsMTRRVCOperationalStateClusterErrorStateStruct mtrrvcOperationalStateClusterErrorStateStruct, IsNSNumber value) => mtrrvcOperationalStateClusterErrorStateStruct -> value -> IO ()
setErrorStateID mtrrvcOperationalStateClusterErrorStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcOperationalStateClusterErrorStateStruct (mkSelector "setErrorStateID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- errorStateLabel@
errorStateLabel :: IsMTRRVCOperationalStateClusterErrorStateStruct mtrrvcOperationalStateClusterErrorStateStruct => mtrrvcOperationalStateClusterErrorStateStruct -> IO (Id NSString)
errorStateLabel mtrrvcOperationalStateClusterErrorStateStruct  =
    sendMsg mtrrvcOperationalStateClusterErrorStateStruct (mkSelector "errorStateLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setErrorStateLabel:@
setErrorStateLabel :: (IsMTRRVCOperationalStateClusterErrorStateStruct mtrrvcOperationalStateClusterErrorStateStruct, IsNSString value) => mtrrvcOperationalStateClusterErrorStateStruct -> value -> IO ()
setErrorStateLabel mtrrvcOperationalStateClusterErrorStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcOperationalStateClusterErrorStateStruct (mkSelector "setErrorStateLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- errorStateDetails@
errorStateDetails :: IsMTRRVCOperationalStateClusterErrorStateStruct mtrrvcOperationalStateClusterErrorStateStruct => mtrrvcOperationalStateClusterErrorStateStruct -> IO (Id NSString)
errorStateDetails mtrrvcOperationalStateClusterErrorStateStruct  =
    sendMsg mtrrvcOperationalStateClusterErrorStateStruct (mkSelector "errorStateDetails") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setErrorStateDetails:@
setErrorStateDetails :: (IsMTRRVCOperationalStateClusterErrorStateStruct mtrrvcOperationalStateClusterErrorStateStruct, IsNSString value) => mtrrvcOperationalStateClusterErrorStateStruct -> value -> IO ()
setErrorStateDetails mtrrvcOperationalStateClusterErrorStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcOperationalStateClusterErrorStateStruct (mkSelector "setErrorStateDetails:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

