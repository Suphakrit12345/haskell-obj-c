{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREnergyPreferenceClusterBalanceStruct@.
module ObjC.Matter.MTREnergyPreferenceClusterBalanceStruct
  ( MTREnergyPreferenceClusterBalanceStruct
  , IsMTREnergyPreferenceClusterBalanceStruct(..)
  , step
  , setStep
  , label
  , setLabel
  , stepSelector
  , setStepSelector
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

-- | @- step@
step :: IsMTREnergyPreferenceClusterBalanceStruct mtrEnergyPreferenceClusterBalanceStruct => mtrEnergyPreferenceClusterBalanceStruct -> IO (Id NSNumber)
step mtrEnergyPreferenceClusterBalanceStruct  =
    sendMsg mtrEnergyPreferenceClusterBalanceStruct (mkSelector "step") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStep:@
setStep :: (IsMTREnergyPreferenceClusterBalanceStruct mtrEnergyPreferenceClusterBalanceStruct, IsNSNumber value) => mtrEnergyPreferenceClusterBalanceStruct -> value -> IO ()
setStep mtrEnergyPreferenceClusterBalanceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyPreferenceClusterBalanceStruct (mkSelector "setStep:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- label@
label :: IsMTREnergyPreferenceClusterBalanceStruct mtrEnergyPreferenceClusterBalanceStruct => mtrEnergyPreferenceClusterBalanceStruct -> IO (Id NSString)
label mtrEnergyPreferenceClusterBalanceStruct  =
    sendMsg mtrEnergyPreferenceClusterBalanceStruct (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLabel:@
setLabel :: (IsMTREnergyPreferenceClusterBalanceStruct mtrEnergyPreferenceClusterBalanceStruct, IsNSString value) => mtrEnergyPreferenceClusterBalanceStruct -> value -> IO ()
setLabel mtrEnergyPreferenceClusterBalanceStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEnergyPreferenceClusterBalanceStruct (mkSelector "setLabel:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @step@
stepSelector :: Selector
stepSelector = mkSelector "step"

-- | @Selector@ for @setStep:@
setStepSelector :: Selector
setStepSelector = mkSelector "setStep:"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @setLabel:@
setLabelSelector :: Selector
setLabelSelector = mkSelector "setLabel:"

