{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MEHEVCDependencyInfo
--
-- Provides information about the HEVC dependency attributes of a sample.
--
-- An instance of this class is returned by MESampleCursor property hevcDependencyInfo.
--
-- Generated bindings for @MEHEVCDependencyInfo@.
module ObjC.MediaExtension.MEHEVCDependencyInfo
  ( MEHEVCDependencyInfo
  , IsMEHEVCDependencyInfo(..)
  , temporalSubLayerAccess
  , setTemporalSubLayerAccess
  , stepwiseTemporalSubLayerAccess
  , setStepwiseTemporalSubLayerAccess
  , syncSampleNALUnitType
  , setSyncSampleNALUnitType
  , temporalLevel
  , setTemporalLevel
  , profileSpace
  , setProfileSpace
  , tierFlag
  , setTierFlag
  , profileIndex
  , setProfileIndex
  , profileCompatibilityFlags
  , setProfileCompatibilityFlags
  , constraintIndicatorFlags
  , setConstraintIndicatorFlags
  , levelIndex
  , setLevelIndex
  , temporalSubLayerAccessSelector
  , setTemporalSubLayerAccessSelector
  , stepwiseTemporalSubLayerAccessSelector
  , setStepwiseTemporalSubLayerAccessSelector
  , syncSampleNALUnitTypeSelector
  , setSyncSampleNALUnitTypeSelector
  , temporalLevelSelector
  , setTemporalLevelSelector
  , profileSpaceSelector
  , setProfileSpaceSelector
  , tierFlagSelector
  , setTierFlagSelector
  , profileIndexSelector
  , setProfileIndexSelector
  , profileCompatibilityFlagsSelector
  , setProfileCompatibilityFlagsSelector
  , constraintIndicatorFlagsSelector
  , setConstraintIndicatorFlagsSelector
  , levelIndexSelector
  , setLevelIndexSelector


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

import ObjC.MediaExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | temporalSubLayerAccess
--
-- YES if the sample is an HEVC 'TSA' picture, NO otherwise.
--
-- Maps to the kCMSampleAttachmentKey_HEVCTemporalSubLayerAccess sample buffer attachment.
--
-- ObjC selector: @- temporalSubLayerAccess@
temporalSubLayerAccess :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO Bool
temporalSubLayerAccess mehevcDependencyInfo  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mehevcDependencyInfo (mkSelector "temporalSubLayerAccess") retCULong []

-- | temporalSubLayerAccess
--
-- YES if the sample is an HEVC 'TSA' picture, NO otherwise.
--
-- Maps to the kCMSampleAttachmentKey_HEVCTemporalSubLayerAccess sample buffer attachment.
--
-- ObjC selector: @- setTemporalSubLayerAccess:@
setTemporalSubLayerAccess :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> Bool -> IO ()
setTemporalSubLayerAccess mehevcDependencyInfo  value =
    sendMsg mehevcDependencyInfo (mkSelector "setTemporalSubLayerAccess:") retVoid [argCULong (if value then 1 else 0)]

-- | stepwiseTemporalSubLayerAccess
--
-- YES if the sample is an HEVC 'STSA' picture, NO otherwise.
--
-- Maps to the kCMSampleAttachmentKey_HEVCStepwiseTemporalSubLayerAccess sample buffer attachment.
--
-- ObjC selector: @- stepwiseTemporalSubLayerAccess@
stepwiseTemporalSubLayerAccess :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO Bool
stepwiseTemporalSubLayerAccess mehevcDependencyInfo  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mehevcDependencyInfo (mkSelector "stepwiseTemporalSubLayerAccess") retCULong []

-- | stepwiseTemporalSubLayerAccess
--
-- YES if the sample is an HEVC 'STSA' picture, NO otherwise.
--
-- Maps to the kCMSampleAttachmentKey_HEVCStepwiseTemporalSubLayerAccess sample buffer attachment.
--
-- ObjC selector: @- setStepwiseTemporalSubLayerAccess:@
setStepwiseTemporalSubLayerAccess :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> Bool -> IO ()
setStepwiseTemporalSubLayerAccess mehevcDependencyInfo  value =
    sendMsg mehevcDependencyInfo (mkSelector "setStepwiseTemporalSubLayerAccess:") retVoid [argCULong (if value then 1 else 0)]

-- | syncSampleNALUnitType
--
-- The NAL unit type for HEVC 'sync' sample groups, or -1 if this information is not available.
--
-- Maps to the kCMSampleAttachmentKey_HEVCSyncSampleNALUnitType sample buffer attachment.
--
-- ObjC selector: @- syncSampleNALUnitType@
syncSampleNALUnitType :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO CShort
syncSampleNALUnitType mehevcDependencyInfo  =
    fmap fromIntegral $ sendMsg mehevcDependencyInfo (mkSelector "syncSampleNALUnitType") retCInt []

-- | syncSampleNALUnitType
--
-- The NAL unit type for HEVC 'sync' sample groups, or -1 if this information is not available.
--
-- Maps to the kCMSampleAttachmentKey_HEVCSyncSampleNALUnitType sample buffer attachment.
--
-- ObjC selector: @- setSyncSampleNALUnitType:@
setSyncSampleNALUnitType :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> CShort -> IO ()
setSyncSampleNALUnitType mehevcDependencyInfo  value =
    sendMsg mehevcDependencyInfo (mkSelector "setSyncSampleNALUnitType:") retVoid [argCInt (fromIntegral value)]

-- | temporalLevel
--
-- The HEVC temporal level, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_TemporalLevel sample buffer attachment.
--
-- ObjC selector: @- temporalLevel@
temporalLevel :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO CShort
temporalLevel mehevcDependencyInfo  =
    fmap fromIntegral $ sendMsg mehevcDependencyInfo (mkSelector "temporalLevel") retCInt []

-- | temporalLevel
--
-- The HEVC temporal level, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_TemporalLevel sample buffer attachment.
--
-- ObjC selector: @- setTemporalLevel:@
setTemporalLevel :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> CShort -> IO ()
setTemporalLevel mehevcDependencyInfo  value =
    sendMsg mehevcDependencyInfo (mkSelector "setTemporalLevel:") retVoid [argCInt (fromIntegral value)]

-- | profileSpace
--
-- The HEVC profile space, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_ProfileSpace sample buffer attachment.
--
-- ObjC selector: @- profileSpace@
profileSpace :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO CShort
profileSpace mehevcDependencyInfo  =
    fmap fromIntegral $ sendMsg mehevcDependencyInfo (mkSelector "profileSpace") retCInt []

-- | profileSpace
--
-- The HEVC profile space, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_ProfileSpace sample buffer attachment.
--
-- ObjC selector: @- setProfileSpace:@
setProfileSpace :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> CShort -> IO ()
setProfileSpace mehevcDependencyInfo  value =
    sendMsg mehevcDependencyInfo (mkSelector "setProfileSpace:") retVoid [argCInt (fromIntegral value)]

-- | tierFlag
--
-- The HEVC tier level flag, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_TierFlag sample buffer attachment.
--
-- ObjC selector: @- tierFlag@
tierFlag :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO CShort
tierFlag mehevcDependencyInfo  =
    fmap fromIntegral $ sendMsg mehevcDependencyInfo (mkSelector "tierFlag") retCInt []

-- | tierFlag
--
-- The HEVC tier level flag, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_TierFlag sample buffer attachment.
--
-- ObjC selector: @- setTierFlag:@
setTierFlag :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> CShort -> IO ()
setTierFlag mehevcDependencyInfo  value =
    sendMsg mehevcDependencyInfo (mkSelector "setTierFlag:") retVoid [argCInt (fromIntegral value)]

-- | profileIndex
--
-- The HEVC profile index, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_ProfileIndex sample buffer attachment.
--
-- ObjC selector: @- profileIndex@
profileIndex :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO CShort
profileIndex mehevcDependencyInfo  =
    fmap fromIntegral $ sendMsg mehevcDependencyInfo (mkSelector "profileIndex") retCInt []

-- | profileIndex
--
-- The HEVC profile index, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_ProfileIndex sample buffer attachment.
--
-- ObjC selector: @- setProfileIndex:@
setProfileIndex :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> CShort -> IO ()
setProfileIndex mehevcDependencyInfo  value =
    sendMsg mehevcDependencyInfo (mkSelector "setProfileIndex:") retVoid [argCInt (fromIntegral value)]

-- | profileCompatibilityFlags
--
-- The HEVC profile compatibility flags (4 bytes), or nil of this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_ProfileCompatibilityFlags sample buffer attachment.
--
-- ObjC selector: @- profileCompatibilityFlags@
profileCompatibilityFlags :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO (Id NSData)
profileCompatibilityFlags mehevcDependencyInfo  =
    sendMsg mehevcDependencyInfo (mkSelector "profileCompatibilityFlags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | profileCompatibilityFlags
--
-- The HEVC profile compatibility flags (4 bytes), or nil of this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_ProfileCompatibilityFlags sample buffer attachment.
--
-- ObjC selector: @- setProfileCompatibilityFlags:@
setProfileCompatibilityFlags :: (IsMEHEVCDependencyInfo mehevcDependencyInfo, IsNSData value) => mehevcDependencyInfo -> value -> IO ()
setProfileCompatibilityFlags mehevcDependencyInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mehevcDependencyInfo (mkSelector "setProfileCompatibilityFlags:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | constraintIndicatorFlags
--
-- The HEVC constraint indicator flags (6 bytes), or nil of this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_ConstraintIndicatorFlags sample buffer attachment.
--
-- ObjC selector: @- constraintIndicatorFlags@
constraintIndicatorFlags :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO (Id NSData)
constraintIndicatorFlags mehevcDependencyInfo  =
    sendMsg mehevcDependencyInfo (mkSelector "constraintIndicatorFlags") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | constraintIndicatorFlags
--
-- The HEVC constraint indicator flags (6 bytes), or nil of this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_ConstraintIndicatorFlags sample buffer attachment.
--
-- ObjC selector: @- setConstraintIndicatorFlags:@
setConstraintIndicatorFlags :: (IsMEHEVCDependencyInfo mehevcDependencyInfo, IsNSData value) => mehevcDependencyInfo -> value -> IO ()
setConstraintIndicatorFlags mehevcDependencyInfo  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mehevcDependencyInfo (mkSelector "setConstraintIndicatorFlags:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | levelIndex
--
-- The HEVC level index, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_LevelIndex sample buffer attachment.
--
-- ObjC selector: @- levelIndex@
levelIndex :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> IO CShort
levelIndex mehevcDependencyInfo  =
    fmap fromIntegral $ sendMsg mehevcDependencyInfo (mkSelector "levelIndex") retCInt []

-- | levelIndex
--
-- The HEVC level index, or -1 if this information is not available.
--
-- Maps to the kCMHEVCTemporalLevelInfoKey_LevelIndex sample buffer attachment.
--
-- ObjC selector: @- setLevelIndex:@
setLevelIndex :: IsMEHEVCDependencyInfo mehevcDependencyInfo => mehevcDependencyInfo -> CShort -> IO ()
setLevelIndex mehevcDependencyInfo  value =
    sendMsg mehevcDependencyInfo (mkSelector "setLevelIndex:") retVoid [argCInt (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @temporalSubLayerAccess@
temporalSubLayerAccessSelector :: Selector
temporalSubLayerAccessSelector = mkSelector "temporalSubLayerAccess"

-- | @Selector@ for @setTemporalSubLayerAccess:@
setTemporalSubLayerAccessSelector :: Selector
setTemporalSubLayerAccessSelector = mkSelector "setTemporalSubLayerAccess:"

-- | @Selector@ for @stepwiseTemporalSubLayerAccess@
stepwiseTemporalSubLayerAccessSelector :: Selector
stepwiseTemporalSubLayerAccessSelector = mkSelector "stepwiseTemporalSubLayerAccess"

-- | @Selector@ for @setStepwiseTemporalSubLayerAccess:@
setStepwiseTemporalSubLayerAccessSelector :: Selector
setStepwiseTemporalSubLayerAccessSelector = mkSelector "setStepwiseTemporalSubLayerAccess:"

-- | @Selector@ for @syncSampleNALUnitType@
syncSampleNALUnitTypeSelector :: Selector
syncSampleNALUnitTypeSelector = mkSelector "syncSampleNALUnitType"

-- | @Selector@ for @setSyncSampleNALUnitType:@
setSyncSampleNALUnitTypeSelector :: Selector
setSyncSampleNALUnitTypeSelector = mkSelector "setSyncSampleNALUnitType:"

-- | @Selector@ for @temporalLevel@
temporalLevelSelector :: Selector
temporalLevelSelector = mkSelector "temporalLevel"

-- | @Selector@ for @setTemporalLevel:@
setTemporalLevelSelector :: Selector
setTemporalLevelSelector = mkSelector "setTemporalLevel:"

-- | @Selector@ for @profileSpace@
profileSpaceSelector :: Selector
profileSpaceSelector = mkSelector "profileSpace"

-- | @Selector@ for @setProfileSpace:@
setProfileSpaceSelector :: Selector
setProfileSpaceSelector = mkSelector "setProfileSpace:"

-- | @Selector@ for @tierFlag@
tierFlagSelector :: Selector
tierFlagSelector = mkSelector "tierFlag"

-- | @Selector@ for @setTierFlag:@
setTierFlagSelector :: Selector
setTierFlagSelector = mkSelector "setTierFlag:"

-- | @Selector@ for @profileIndex@
profileIndexSelector :: Selector
profileIndexSelector = mkSelector "profileIndex"

-- | @Selector@ for @setProfileIndex:@
setProfileIndexSelector :: Selector
setProfileIndexSelector = mkSelector "setProfileIndex:"

-- | @Selector@ for @profileCompatibilityFlags@
profileCompatibilityFlagsSelector :: Selector
profileCompatibilityFlagsSelector = mkSelector "profileCompatibilityFlags"

-- | @Selector@ for @setProfileCompatibilityFlags:@
setProfileCompatibilityFlagsSelector :: Selector
setProfileCompatibilityFlagsSelector = mkSelector "setProfileCompatibilityFlags:"

-- | @Selector@ for @constraintIndicatorFlags@
constraintIndicatorFlagsSelector :: Selector
constraintIndicatorFlagsSelector = mkSelector "constraintIndicatorFlags"

-- | @Selector@ for @setConstraintIndicatorFlags:@
setConstraintIndicatorFlagsSelector :: Selector
setConstraintIndicatorFlagsSelector = mkSelector "setConstraintIndicatorFlags:"

-- | @Selector@ for @levelIndex@
levelIndexSelector :: Selector
levelIndexSelector = mkSelector "levelIndex"

-- | @Selector@ for @setLevelIndex:@
setLevelIndexSelector :: Selector
setLevelIndexSelector = mkSelector "setLevelIndex:"

