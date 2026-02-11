{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRChimeClusterChimeSoundStruct@.
module ObjC.Matter.MTRChimeClusterChimeSoundStruct
  ( MTRChimeClusterChimeSoundStruct
  , IsMTRChimeClusterChimeSoundStruct(..)
  , chimeID
  , setChimeID
  , name
  , setName
  , chimeIDSelector
  , setChimeIDSelector
  , nameSelector
  , setNameSelector


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

-- | @- chimeID@
chimeID :: IsMTRChimeClusterChimeSoundStruct mtrChimeClusterChimeSoundStruct => mtrChimeClusterChimeSoundStruct -> IO (Id NSNumber)
chimeID mtrChimeClusterChimeSoundStruct  =
    sendMsg mtrChimeClusterChimeSoundStruct (mkSelector "chimeID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setChimeID:@
setChimeID :: (IsMTRChimeClusterChimeSoundStruct mtrChimeClusterChimeSoundStruct, IsNSNumber value) => mtrChimeClusterChimeSoundStruct -> value -> IO ()
setChimeID mtrChimeClusterChimeSoundStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChimeClusterChimeSoundStruct (mkSelector "setChimeID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRChimeClusterChimeSoundStruct mtrChimeClusterChimeSoundStruct => mtrChimeClusterChimeSoundStruct -> IO (Id NSString)
name mtrChimeClusterChimeSoundStruct  =
    sendMsg mtrChimeClusterChimeSoundStruct (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRChimeClusterChimeSoundStruct mtrChimeClusterChimeSoundStruct, IsNSString value) => mtrChimeClusterChimeSoundStruct -> value -> IO ()
setName mtrChimeClusterChimeSoundStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrChimeClusterChimeSoundStruct (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @chimeID@
chimeIDSelector :: Selector
chimeIDSelector = mkSelector "chimeID"

-- | @Selector@ for @setChimeID:@
setChimeIDSelector :: Selector
setChimeIDSelector = mkSelector "setChimeID:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

