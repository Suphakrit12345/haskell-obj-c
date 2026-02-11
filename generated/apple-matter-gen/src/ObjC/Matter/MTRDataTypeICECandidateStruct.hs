{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeICECandidateStruct@.
module ObjC.Matter.MTRDataTypeICECandidateStruct
  ( MTRDataTypeICECandidateStruct
  , IsMTRDataTypeICECandidateStruct(..)
  , candidate
  , setCandidate
  , sdpMid
  , setSdpMid
  , sdpmLineIndex
  , setSdpmLineIndex
  , candidateSelector
  , setCandidateSelector
  , sdpMidSelector
  , setSdpMidSelector
  , sdpmLineIndexSelector
  , setSdpmLineIndexSelector


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

-- | @- candidate@
candidate :: IsMTRDataTypeICECandidateStruct mtrDataTypeICECandidateStruct => mtrDataTypeICECandidateStruct -> IO (Id NSString)
candidate mtrDataTypeICECandidateStruct  =
    sendMsg mtrDataTypeICECandidateStruct (mkSelector "candidate") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCandidate:@
setCandidate :: (IsMTRDataTypeICECandidateStruct mtrDataTypeICECandidateStruct, IsNSString value) => mtrDataTypeICECandidateStruct -> value -> IO ()
setCandidate mtrDataTypeICECandidateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeICECandidateStruct (mkSelector "setCandidate:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sdpMid@
sdpMid :: IsMTRDataTypeICECandidateStruct mtrDataTypeICECandidateStruct => mtrDataTypeICECandidateStruct -> IO (Id NSString)
sdpMid mtrDataTypeICECandidateStruct  =
    sendMsg mtrDataTypeICECandidateStruct (mkSelector "sdpMid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSdpMid:@
setSdpMid :: (IsMTRDataTypeICECandidateStruct mtrDataTypeICECandidateStruct, IsNSString value) => mtrDataTypeICECandidateStruct -> value -> IO ()
setSdpMid mtrDataTypeICECandidateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeICECandidateStruct (mkSelector "setSdpMid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- sdpmLineIndex@
sdpmLineIndex :: IsMTRDataTypeICECandidateStruct mtrDataTypeICECandidateStruct => mtrDataTypeICECandidateStruct -> IO (Id NSNumber)
sdpmLineIndex mtrDataTypeICECandidateStruct  =
    sendMsg mtrDataTypeICECandidateStruct (mkSelector "sdpmLineIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSdpmLineIndex:@
setSdpmLineIndex :: (IsMTRDataTypeICECandidateStruct mtrDataTypeICECandidateStruct, IsNSNumber value) => mtrDataTypeICECandidateStruct -> value -> IO ()
setSdpmLineIndex mtrDataTypeICECandidateStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeICECandidateStruct (mkSelector "setSdpmLineIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @candidate@
candidateSelector :: Selector
candidateSelector = mkSelector "candidate"

-- | @Selector@ for @setCandidate:@
setCandidateSelector :: Selector
setCandidateSelector = mkSelector "setCandidate:"

-- | @Selector@ for @sdpMid@
sdpMidSelector :: Selector
sdpMidSelector = mkSelector "sdpMid"

-- | @Selector@ for @setSdpMid:@
setSdpMidSelector :: Selector
setSdpMidSelector = mkSelector "setSdpMid:"

-- | @Selector@ for @sdpmLineIndex@
sdpmLineIndexSelector :: Selector
sdpmLineIndexSelector = mkSelector "sdpmLineIndex"

-- | @Selector@ for @setSdpmLineIndex:@
setSdpmLineIndexSelector :: Selector
setSdpmLineIndexSelector = mkSelector "setSdpmLineIndex:"

