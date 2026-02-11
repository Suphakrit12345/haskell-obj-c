{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalStateClusterErrorStateStruct@.
module ObjC.Matter.MTROperationalStateClusterErrorStateStruct
  ( MTROperationalStateClusterErrorStateStruct
  , IsMTROperationalStateClusterErrorStateStruct(..)
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
errorStateID :: IsMTROperationalStateClusterErrorStateStruct mtrOperationalStateClusterErrorStateStruct => mtrOperationalStateClusterErrorStateStruct -> IO (Id NSNumber)
errorStateID mtrOperationalStateClusterErrorStateStruct  =
    sendMsg mtrOperationalStateClusterErrorStateStruct (mkSelector "errorStateID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setErrorStateID:@
setErrorStateID :: (IsMTROperationalStateClusterErrorStateStruct mtrOperationalStateClusterErrorStateStruct, IsNSNumber value) => mtrOperationalStateClusterErrorStateStruct -> value -> IO ()
setErrorStateID mtrOperationalStateClusterErrorStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalStateClusterErrorStateStruct (mkSelector "setErrorStateID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- errorStateLabel@
errorStateLabel :: IsMTROperationalStateClusterErrorStateStruct mtrOperationalStateClusterErrorStateStruct => mtrOperationalStateClusterErrorStateStruct -> IO (Id NSString)
errorStateLabel mtrOperationalStateClusterErrorStateStruct  =
    sendMsg mtrOperationalStateClusterErrorStateStruct (mkSelector "errorStateLabel") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setErrorStateLabel:@
setErrorStateLabel :: (IsMTROperationalStateClusterErrorStateStruct mtrOperationalStateClusterErrorStateStruct, IsNSString value) => mtrOperationalStateClusterErrorStateStruct -> value -> IO ()
setErrorStateLabel mtrOperationalStateClusterErrorStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalStateClusterErrorStateStruct (mkSelector "setErrorStateLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- errorStateDetails@
errorStateDetails :: IsMTROperationalStateClusterErrorStateStruct mtrOperationalStateClusterErrorStateStruct => mtrOperationalStateClusterErrorStateStruct -> IO (Id NSString)
errorStateDetails mtrOperationalStateClusterErrorStateStruct  =
    sendMsg mtrOperationalStateClusterErrorStateStruct (mkSelector "errorStateDetails") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setErrorStateDetails:@
setErrorStateDetails :: (IsMTROperationalStateClusterErrorStateStruct mtrOperationalStateClusterErrorStateStruct, IsNSString value) => mtrOperationalStateClusterErrorStateStruct -> value -> IO ()
setErrorStateDetails mtrOperationalStateClusterErrorStateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalStateClusterErrorStateStruct (mkSelector "setErrorStateDetails:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

