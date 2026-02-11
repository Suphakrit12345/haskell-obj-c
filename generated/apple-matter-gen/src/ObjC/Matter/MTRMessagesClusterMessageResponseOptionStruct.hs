{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRMessagesClusterMessageResponseOptionStruct@.
module ObjC.Matter.MTRMessagesClusterMessageResponseOptionStruct
  ( MTRMessagesClusterMessageResponseOptionStruct
  , IsMTRMessagesClusterMessageResponseOptionStruct(..)
  , messageResponseID
  , setMessageResponseID
  , label
  , setLabel
  , messageResponseIDSelector
  , setMessageResponseIDSelector
  , labelSelector
  , setLabelSelector


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

-- | @- messageResponseID@
messageResponseID :: IsMTRMessagesClusterMessageResponseOptionStruct mtrMessagesClusterMessageResponseOptionStruct => mtrMessagesClusterMessageResponseOptionStruct -> IO (Id NSNumber)
messageResponseID mtrMessagesClusterMessageResponseOptionStruct  =
    sendMsg mtrMessagesClusterMessageResponseOptionStruct (mkSelector "messageResponseID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMessageResponseID:@
setMessageResponseID :: (IsMTRMessagesClusterMessageResponseOptionStruct mtrMessagesClusterMessageResponseOptionStruct, IsNSNumber value) => mtrMessagesClusterMessageResponseOptionStruct -> value -> IO ()
setMessageResponseID mtrMessagesClusterMessageResponseOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterMessageResponseOptionStruct (mkSelector "setMessageResponseID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- label@
label :: IsMTRMessagesClusterMessageResponseOptionStruct mtrMessagesClusterMessageResponseOptionStruct => mtrMessagesClusterMessageResponseOptionStruct -> IO (Id NSString)
label mtrMessagesClusterMessageResponseOptionStruct  =
    sendMsg mtrMessagesClusterMessageResponseOptionStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTRMessagesClusterMessageResponseOptionStruct mtrMessagesClusterMessageResponseOptionStruct, IsNSString value) => mtrMessagesClusterMessageResponseOptionStruct -> value -> IO ()
setLabel mtrMessagesClusterMessageResponseOptionStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrMessagesClusterMessageResponseOptionStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @messageResponseID@
messageResponseIDSelector :: Selector
messageResponseIDSelector = mkSelector "messageResponseID"

-- | @Selector@ for @setMessageResponseID:@
setMessageResponseIDSelector :: Selector
setMessageResponseIDSelector = mkSelector "setMessageResponseID:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

