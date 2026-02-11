{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSValue@.
module ObjC.SceneKit.NSValue
  ( NSValue
  , IsNSValue(..)
  , scnVector3Value
  , scnVector4Value
  , scnMatrix4Value
  , scnVector3ValueSelector
  , scnVector4ValueSelector
  , scnMatrix4ValueSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.SceneKit.Internal.Classes
import ObjC.SceneKit.Internal.Structs

-- | @- SCNVector3Value@
scnVector3Value :: IsNSValue nsValue => nsValue -> IO SCNVector3
scnVector3Value nsValue  =
    sendMsgStret nsValue (mkSelector "SCNVector3Value") retSCNVector3 []

-- | @- SCNVector4Value@
scnVector4Value :: IsNSValue nsValue => nsValue -> IO SCNVector4
scnVector4Value nsValue  =
    sendMsgStret nsValue (mkSelector "SCNVector4Value") retSCNVector4 []

-- | @- SCNMatrix4Value@
scnMatrix4Value :: IsNSValue nsValue => nsValue -> IO SCNMatrix4
scnMatrix4Value nsValue  =
    sendMsgStret nsValue (mkSelector "SCNMatrix4Value") retSCNMatrix4 []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @SCNVector3Value@
scnVector3ValueSelector :: Selector
scnVector3ValueSelector = mkSelector "SCNVector3Value"

-- | @Selector@ for @SCNVector4Value@
scnVector4ValueSelector :: Selector
scnVector4ValueSelector = mkSelector "SCNVector4Value"

-- | @Selector@ for @SCNMatrix4Value@
scnMatrix4ValueSelector :: Selector
scnMatrix4ValueSelector = mkSelector "SCNMatrix4Value"

