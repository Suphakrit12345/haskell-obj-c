{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTestClusterClusterTestListStructOctet@.
module ObjC.Matter.MTRTestClusterClusterTestListStructOctet
  ( MTRTestClusterClusterTestListStructOctet
  , IsMTRTestClusterClusterTestListStructOctet(..)
  , member1
  , setMember1
  , member2
  , setMember2
  , member1Selector
  , setMember1Selector
  , member2Selector
  , setMember2Selector


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

-- | @- member1@
member1 :: IsMTRTestClusterClusterTestListStructOctet mtrTestClusterClusterTestListStructOctet => mtrTestClusterClusterTestListStructOctet -> IO (Id NSNumber)
member1 mtrTestClusterClusterTestListStructOctet  =
    sendMsg mtrTestClusterClusterTestListStructOctet (mkSelector "member1") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMember1:@
setMember1 :: (IsMTRTestClusterClusterTestListStructOctet mtrTestClusterClusterTestListStructOctet, IsNSNumber value) => mtrTestClusterClusterTestListStructOctet -> value -> IO ()
setMember1 mtrTestClusterClusterTestListStructOctet  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestListStructOctet (mkSelector "setMember1:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- member2@
member2 :: IsMTRTestClusterClusterTestListStructOctet mtrTestClusterClusterTestListStructOctet => mtrTestClusterClusterTestListStructOctet -> IO (Id NSData)
member2 mtrTestClusterClusterTestListStructOctet  =
    sendMsg mtrTestClusterClusterTestListStructOctet (mkSelector "member2") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setMember2:@
setMember2 :: (IsMTRTestClusterClusterTestListStructOctet mtrTestClusterClusterTestListStructOctet, IsNSData value) => mtrTestClusterClusterTestListStructOctet -> value -> IO ()
setMember2 mtrTestClusterClusterTestListStructOctet  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTestClusterClusterTestListStructOctet (mkSelector "setMember2:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @member1@
member1Selector :: Selector
member1Selector = mkSelector "member1"

-- | @Selector@ for @setMember1:@
setMember1Selector :: Selector
setMember1Selector = mkSelector "setMember1:"

-- | @Selector@ for @member2@
member2Selector :: Selector
member2Selector = mkSelector "member2"

-- | @Selector@ for @setMember2:@
setMember2Selector :: Selector
setMember2Selector = mkSelector "setMember2:"

