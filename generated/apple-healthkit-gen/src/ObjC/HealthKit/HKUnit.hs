{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @HKUnit@.
module ObjC.HealthKit.HKUnit
  ( HKUnit
  , IsHKUnit(..)
  , init_
  , unitFromString
  , unitFromMassFormatterUnit
  , massFormatterUnitFromUnit
  , unitFromLengthFormatterUnit
  , lengthFormatterUnitFromUnit
  , unitFromEnergyFormatterUnit
  , energyFormatterUnitFromUnit
  , isNull
  , appleEffortScoreUnit
  , luxUnitWithMetricPrefix
  , luxUnit
  , radianAngleUnitWithMetricPrefix
  , radianAngleUnit
  , degreeAngleUnit
  , diopterUnit
  , prismDiopterUnit
  , wattUnitWithMetricPrefix
  , wattUnit
  , voltUnitWithMetricPrefix
  , voltUnit
  , hertzUnitWithMetricPrefix
  , hertzUnit
  , unitMultipliedByUnit
  , unitDividedByUnit
  , unitRaisedToPower
  , reciprocalUnit
  , decibelHearingLevelUnit
  , countUnit
  , percentUnit
  , internationalUnit
  , siemenUnitWithMetricPrefix
  , siemenUnit
  , degreeCelsiusUnit
  , degreeFahrenheitUnit
  , kelvinUnit
  , jouleUnitWithMetricPrefix
  , jouleUnit
  , kilocalorieUnit
  , smallCalorieUnit
  , largeCalorieUnit
  , calorieUnit
  , secondUnitWithMetricPrefix
  , secondUnit
  , minuteUnit
  , hourUnit
  , dayUnit
  , pascalUnitWithMetricPrefix
  , pascalUnit
  , millimeterOfMercuryUnit
  , centimeterOfWaterUnit
  , atmosphereUnit
  , decibelAWeightedSoundPressureLevelUnit
  , inchesOfMercuryUnit
  , literUnitWithMetricPrefix
  , literUnit
  , fluidOunceUSUnit
  , fluidOunceImperialUnit
  , pintUSUnit
  , pintImperialUnit
  , cupUSUnit
  , cupImperialUnit
  , meterUnitWithMetricPrefix
  , meterUnit
  , inchUnit
  , footUnit
  , yardUnit
  , mileUnit
  , gramUnitWithMetricPrefix
  , gramUnit
  , ounceUnit
  , poundUnit
  , stoneUnit
  , moleUnitWithMetricPrefix_molarMass
  , moleUnitWithMolarMass
  , unitString
  , initSelector
  , unitFromStringSelector
  , unitFromMassFormatterUnitSelector
  , massFormatterUnitFromUnitSelector
  , unitFromLengthFormatterUnitSelector
  , lengthFormatterUnitFromUnitSelector
  , unitFromEnergyFormatterUnitSelector
  , energyFormatterUnitFromUnitSelector
  , isNullSelector
  , appleEffortScoreUnitSelector
  , luxUnitWithMetricPrefixSelector
  , luxUnitSelector
  , radianAngleUnitWithMetricPrefixSelector
  , radianAngleUnitSelector
  , degreeAngleUnitSelector
  , diopterUnitSelector
  , prismDiopterUnitSelector
  , wattUnitWithMetricPrefixSelector
  , wattUnitSelector
  , voltUnitWithMetricPrefixSelector
  , voltUnitSelector
  , hertzUnitWithMetricPrefixSelector
  , hertzUnitSelector
  , unitMultipliedByUnitSelector
  , unitDividedByUnitSelector
  , unitRaisedToPowerSelector
  , reciprocalUnitSelector
  , decibelHearingLevelUnitSelector
  , countUnitSelector
  , percentUnitSelector
  , internationalUnitSelector
  , siemenUnitWithMetricPrefixSelector
  , siemenUnitSelector
  , degreeCelsiusUnitSelector
  , degreeFahrenheitUnitSelector
  , kelvinUnitSelector
  , jouleUnitWithMetricPrefixSelector
  , jouleUnitSelector
  , kilocalorieUnitSelector
  , smallCalorieUnitSelector
  , largeCalorieUnitSelector
  , calorieUnitSelector
  , secondUnitWithMetricPrefixSelector
  , secondUnitSelector
  , minuteUnitSelector
  , hourUnitSelector
  , dayUnitSelector
  , pascalUnitWithMetricPrefixSelector
  , pascalUnitSelector
  , millimeterOfMercuryUnitSelector
  , centimeterOfWaterUnitSelector
  , atmosphereUnitSelector
  , decibelAWeightedSoundPressureLevelUnitSelector
  , inchesOfMercuryUnitSelector
  , literUnitWithMetricPrefixSelector
  , literUnitSelector
  , fluidOunceUSUnitSelector
  , fluidOunceImperialUnitSelector
  , pintUSUnitSelector
  , pintImperialUnitSelector
  , cupUSUnitSelector
  , cupImperialUnitSelector
  , meterUnitWithMetricPrefixSelector
  , meterUnitSelector
  , inchUnitSelector
  , footUnitSelector
  , yardUnitSelector
  , mileUnitSelector
  , gramUnitWithMetricPrefixSelector
  , gramUnitSelector
  , ounceUnitSelector
  , poundUnitSelector
  , stoneUnitSelector
  , moleUnitWithMetricPrefix_molarMassSelector
  , moleUnitWithMolarMassSelector
  , unitStringSelector

  -- * Enum types
  , HKMetricPrefix(HKMetricPrefix)
  , pattern HKMetricPrefixNone
  , pattern HKMetricPrefixFemto
  , pattern HKMetricPrefixPico
  , pattern HKMetricPrefixNano
  , pattern HKMetricPrefixMicro
  , pattern HKMetricPrefixMilli
  , pattern HKMetricPrefixCenti
  , pattern HKMetricPrefixDeci
  , pattern HKMetricPrefixDeca
  , pattern HKMetricPrefixHecto
  , pattern HKMetricPrefixKilo
  , pattern HKMetricPrefixMega
  , pattern HKMetricPrefixGiga
  , pattern HKMetricPrefixTera
  , NSEnergyFormatterUnit(NSEnergyFormatterUnit)
  , pattern NSEnergyFormatterUnitJoule
  , pattern NSEnergyFormatterUnitKilojoule
  , pattern NSEnergyFormatterUnitCalorie
  , pattern NSEnergyFormatterUnitKilocalorie
  , NSLengthFormatterUnit(NSLengthFormatterUnit)
  , pattern NSLengthFormatterUnitMillimeter
  , pattern NSLengthFormatterUnitCentimeter
  , pattern NSLengthFormatterUnitMeter
  , pattern NSLengthFormatterUnitKilometer
  , pattern NSLengthFormatterUnitInch
  , pattern NSLengthFormatterUnitFoot
  , pattern NSLengthFormatterUnitYard
  , pattern NSLengthFormatterUnitMile
  , NSMassFormatterUnit(NSMassFormatterUnit)
  , pattern NSMassFormatterUnitGram
  , pattern NSMassFormatterUnitKilogram
  , pattern NSMassFormatterUnitOunce
  , pattern NSMassFormatterUnitPound
  , pattern NSMassFormatterUnitStone

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

import ObjC.HealthKit.Internal.Classes
import ObjC.HealthKit.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsHKUnit hkUnit => hkUnit -> IO (Id HKUnit)
init_ hkUnit  =
    sendMsg hkUnit (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ unitFromString:@
unitFromString :: IsNSString string => string -> IO (Id HKUnit)
unitFromString string =
  do
    cls' <- getRequiredClass "HKUnit"
    withObjCPtr string $ \raw_string ->
      sendClassMsg cls' (mkSelector "unitFromString:") (retPtr retVoid) [argPtr (castPtr raw_string :: Ptr ())] >>= retainedObject . castPtr

-- | @+ unitFromMassFormatterUnit:@
unitFromMassFormatterUnit :: NSMassFormatterUnit -> IO (Id HKUnit)
unitFromMassFormatterUnit massFormatterUnit =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "unitFromMassFormatterUnit:") (retPtr retVoid) [argCLong (coerce massFormatterUnit)] >>= retainedObject . castPtr

-- | @+ massFormatterUnitFromUnit:@
massFormatterUnitFromUnit :: IsHKUnit unit => unit -> IO NSMassFormatterUnit
massFormatterUnitFromUnit unit =
  do
    cls' <- getRequiredClass "HKUnit"
    withObjCPtr unit $ \raw_unit ->
      fmap (coerce :: CLong -> NSMassFormatterUnit) $ sendClassMsg cls' (mkSelector "massFormatterUnitFromUnit:") retCLong [argPtr (castPtr raw_unit :: Ptr ())]

-- | @+ unitFromLengthFormatterUnit:@
unitFromLengthFormatterUnit :: NSLengthFormatterUnit -> IO (Id HKUnit)
unitFromLengthFormatterUnit lengthFormatterUnit =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "unitFromLengthFormatterUnit:") (retPtr retVoid) [argCLong (coerce lengthFormatterUnit)] >>= retainedObject . castPtr

-- | @+ lengthFormatterUnitFromUnit:@
lengthFormatterUnitFromUnit :: IsHKUnit unit => unit -> IO NSLengthFormatterUnit
lengthFormatterUnitFromUnit unit =
  do
    cls' <- getRequiredClass "HKUnit"
    withObjCPtr unit $ \raw_unit ->
      fmap (coerce :: CLong -> NSLengthFormatterUnit) $ sendClassMsg cls' (mkSelector "lengthFormatterUnitFromUnit:") retCLong [argPtr (castPtr raw_unit :: Ptr ())]

-- | @+ unitFromEnergyFormatterUnit:@
unitFromEnergyFormatterUnit :: NSEnergyFormatterUnit -> IO (Id HKUnit)
unitFromEnergyFormatterUnit energyFormatterUnit =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "unitFromEnergyFormatterUnit:") (retPtr retVoid) [argCLong (coerce energyFormatterUnit)] >>= retainedObject . castPtr

-- | @+ energyFormatterUnitFromUnit:@
energyFormatterUnitFromUnit :: IsHKUnit unit => unit -> IO NSEnergyFormatterUnit
energyFormatterUnitFromUnit unit =
  do
    cls' <- getRequiredClass "HKUnit"
    withObjCPtr unit $ \raw_unit ->
      fmap (coerce :: CLong -> NSEnergyFormatterUnit) $ sendClassMsg cls' (mkSelector "energyFormatterUnitFromUnit:") retCLong [argPtr (castPtr raw_unit :: Ptr ())]

-- | @- isNull@
isNull :: IsHKUnit hkUnit => hkUnit -> IO Bool
isNull hkUnit  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg hkUnit (mkSelector "isNull") retCULong []

-- | @+ appleEffortScoreUnit@
appleEffortScoreUnit :: IO (Id HKUnit)
appleEffortScoreUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "appleEffortScoreUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ luxUnitWithMetricPrefix:@
luxUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
luxUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "luxUnitWithMetricPrefix:") (retPtr retVoid) [argCLong (coerce prefix)] >>= retainedObject . castPtr

-- | @+ luxUnit@
luxUnit :: IO (Id HKUnit)
luxUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "luxUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ radianAngleUnitWithMetricPrefix:@
radianAngleUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
radianAngleUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "radianAngleUnitWithMetricPrefix:") (retPtr retVoid) [argCLong (coerce prefix)] >>= retainedObject . castPtr

-- | @+ radianAngleUnit@
radianAngleUnit :: IO (Id HKUnit)
radianAngleUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "radianAngleUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ degreeAngleUnit@
degreeAngleUnit :: IO (Id HKUnit)
degreeAngleUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "degreeAngleUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ diopterUnit@
diopterUnit :: IO (Id HKUnit)
diopterUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "diopterUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ prismDiopterUnit@
prismDiopterUnit :: IO (Id HKUnit)
prismDiopterUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "prismDiopterUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ wattUnitWithMetricPrefix:@
wattUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
wattUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "wattUnitWithMetricPrefix:") (retPtr retVoid) [argCLong (coerce prefix)] >>= retainedObject . castPtr

-- | @+ wattUnit@
wattUnit :: IO (Id HKUnit)
wattUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "wattUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ voltUnitWithMetricPrefix:@
voltUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
voltUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "voltUnitWithMetricPrefix:") (retPtr retVoid) [argCLong (coerce prefix)] >>= retainedObject . castPtr

-- | @+ voltUnit@
voltUnit :: IO (Id HKUnit)
voltUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "voltUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ hertzUnitWithMetricPrefix:@
hertzUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
hertzUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "hertzUnitWithMetricPrefix:") (retPtr retVoid) [argCLong (coerce prefix)] >>= retainedObject . castPtr

-- | @+ hertzUnit@
hertzUnit :: IO (Id HKUnit)
hertzUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "hertzUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- unitMultipliedByUnit:@
unitMultipliedByUnit :: (IsHKUnit hkUnit, IsHKUnit unit) => hkUnit -> unit -> IO (Id HKUnit)
unitMultipliedByUnit hkUnit  unit =
  withObjCPtr unit $ \raw_unit ->
      sendMsg hkUnit (mkSelector "unitMultipliedByUnit:") (retPtr retVoid) [argPtr (castPtr raw_unit :: Ptr ())] >>= retainedObject . castPtr

-- | @- unitDividedByUnit:@
unitDividedByUnit :: (IsHKUnit hkUnit, IsHKUnit unit) => hkUnit -> unit -> IO (Id HKUnit)
unitDividedByUnit hkUnit  unit =
  withObjCPtr unit $ \raw_unit ->
      sendMsg hkUnit (mkSelector "unitDividedByUnit:") (retPtr retVoid) [argPtr (castPtr raw_unit :: Ptr ())] >>= retainedObject . castPtr

-- | @- unitRaisedToPower:@
unitRaisedToPower :: IsHKUnit hkUnit => hkUnit -> CLong -> IO (Id HKUnit)
unitRaisedToPower hkUnit  power =
    sendMsg hkUnit (mkSelector "unitRaisedToPower:") (retPtr retVoid) [argCLong power] >>= retainedObject . castPtr

-- | @- reciprocalUnit@
reciprocalUnit :: IsHKUnit hkUnit => hkUnit -> IO (Id HKUnit)
reciprocalUnit hkUnit  =
    sendMsg hkUnit (mkSelector "reciprocalUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ decibelHearingLevelUnit@
decibelHearingLevelUnit :: IO (Id HKUnit)
decibelHearingLevelUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "decibelHearingLevelUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ countUnit@
countUnit :: IO (Id HKUnit)
countUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "countUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ percentUnit@
percentUnit :: IO (Id HKUnit)
percentUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "percentUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ internationalUnit@
internationalUnit :: IO (Id HKUnit)
internationalUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "internationalUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ siemenUnitWithMetricPrefix:@
siemenUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
siemenUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "siemenUnitWithMetricPrefix:") (retPtr retVoid) [argCLong (coerce prefix)] >>= retainedObject . castPtr

-- | @+ siemenUnit@
siemenUnit :: IO (Id HKUnit)
siemenUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "siemenUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ degreeCelsiusUnit@
degreeCelsiusUnit :: IO (Id HKUnit)
degreeCelsiusUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "degreeCelsiusUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ degreeFahrenheitUnit@
degreeFahrenheitUnit :: IO (Id HKUnit)
degreeFahrenheitUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "degreeFahrenheitUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kelvinUnit@
kelvinUnit :: IO (Id HKUnit)
kelvinUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "kelvinUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ jouleUnitWithMetricPrefix:@
jouleUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
jouleUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "jouleUnitWithMetricPrefix:") (retPtr retVoid) [argCLong (coerce prefix)] >>= retainedObject . castPtr

-- | @+ jouleUnit@
jouleUnit :: IO (Id HKUnit)
jouleUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "jouleUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ kilocalorieUnit@
kilocalorieUnit :: IO (Id HKUnit)
kilocalorieUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "kilocalorieUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ smallCalorieUnit@
smallCalorieUnit :: IO (Id HKUnit)
smallCalorieUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "smallCalorieUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ largeCalorieUnit@
largeCalorieUnit :: IO (Id HKUnit)
largeCalorieUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "largeCalorieUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ calorieUnit@
calorieUnit :: IO (Id HKUnit)
calorieUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "calorieUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ secondUnitWithMetricPrefix:@
secondUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
secondUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "secondUnitWithMetricPrefix:") (retPtr retVoid) [argCLong (coerce prefix)] >>= retainedObject . castPtr

-- | @+ secondUnit@
secondUnit :: IO (Id HKUnit)
secondUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "secondUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ minuteUnit@
minuteUnit :: IO (Id HKUnit)
minuteUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "minuteUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ hourUnit@
hourUnit :: IO (Id HKUnit)
hourUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "hourUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ dayUnit@
dayUnit :: IO (Id HKUnit)
dayUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "dayUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ pascalUnitWithMetricPrefix:@
pascalUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
pascalUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "pascalUnitWithMetricPrefix:") (retPtr retVoid) [argCLong (coerce prefix)] >>= retainedObject . castPtr

-- | @+ pascalUnit@
pascalUnit :: IO (Id HKUnit)
pascalUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "pascalUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ millimeterOfMercuryUnit@
millimeterOfMercuryUnit :: IO (Id HKUnit)
millimeterOfMercuryUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "millimeterOfMercuryUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ centimeterOfWaterUnit@
centimeterOfWaterUnit :: IO (Id HKUnit)
centimeterOfWaterUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "centimeterOfWaterUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ atmosphereUnit@
atmosphereUnit :: IO (Id HKUnit)
atmosphereUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "atmosphereUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ decibelAWeightedSoundPressureLevelUnit@
decibelAWeightedSoundPressureLevelUnit :: IO (Id HKUnit)
decibelAWeightedSoundPressureLevelUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "decibelAWeightedSoundPressureLevelUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ inchesOfMercuryUnit@
inchesOfMercuryUnit :: IO (Id HKUnit)
inchesOfMercuryUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "inchesOfMercuryUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ literUnitWithMetricPrefix:@
literUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
literUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "literUnitWithMetricPrefix:") (retPtr retVoid) [argCLong (coerce prefix)] >>= retainedObject . castPtr

-- | @+ literUnit@
literUnit :: IO (Id HKUnit)
literUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "literUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ fluidOunceUSUnit@
fluidOunceUSUnit :: IO (Id HKUnit)
fluidOunceUSUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "fluidOunceUSUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ fluidOunceImperialUnit@
fluidOunceImperialUnit :: IO (Id HKUnit)
fluidOunceImperialUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "fluidOunceImperialUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ pintUSUnit@
pintUSUnit :: IO (Id HKUnit)
pintUSUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "pintUSUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ pintImperialUnit@
pintImperialUnit :: IO (Id HKUnit)
pintImperialUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "pintImperialUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ cupUSUnit@
cupUSUnit :: IO (Id HKUnit)
cupUSUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "cupUSUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ cupImperialUnit@
cupImperialUnit :: IO (Id HKUnit)
cupImperialUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "cupImperialUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ meterUnitWithMetricPrefix:@
meterUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
meterUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "meterUnitWithMetricPrefix:") (retPtr retVoid) [argCLong (coerce prefix)] >>= retainedObject . castPtr

-- | @+ meterUnit@
meterUnit :: IO (Id HKUnit)
meterUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "meterUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ inchUnit@
inchUnit :: IO (Id HKUnit)
inchUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "inchUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ footUnit@
footUnit :: IO (Id HKUnit)
footUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "footUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ yardUnit@
yardUnit :: IO (Id HKUnit)
yardUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "yardUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ mileUnit@
mileUnit :: IO (Id HKUnit)
mileUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "mileUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ gramUnitWithMetricPrefix:@
gramUnitWithMetricPrefix :: HKMetricPrefix -> IO (Id HKUnit)
gramUnitWithMetricPrefix prefix =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "gramUnitWithMetricPrefix:") (retPtr retVoid) [argCLong (coerce prefix)] >>= retainedObject . castPtr

-- | @+ gramUnit@
gramUnit :: IO (Id HKUnit)
gramUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "gramUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ ounceUnit@
ounceUnit :: IO (Id HKUnit)
ounceUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "ounceUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ poundUnit@
poundUnit :: IO (Id HKUnit)
poundUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "poundUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ stoneUnit@
stoneUnit :: IO (Id HKUnit)
stoneUnit  =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "stoneUnit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ moleUnitWithMetricPrefix:molarMass:@
moleUnitWithMetricPrefix_molarMass :: HKMetricPrefix -> CDouble -> IO (Id HKUnit)
moleUnitWithMetricPrefix_molarMass prefix gramsPerMole =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "moleUnitWithMetricPrefix:molarMass:") (retPtr retVoid) [argCLong (coerce prefix), argCDouble gramsPerMole] >>= retainedObject . castPtr

-- | @+ moleUnitWithMolarMass:@
moleUnitWithMolarMass :: CDouble -> IO (Id HKUnit)
moleUnitWithMolarMass gramsPerMole =
  do
    cls' <- getRequiredClass "HKUnit"
    sendClassMsg cls' (mkSelector "moleUnitWithMolarMass:") (retPtr retVoid) [argCDouble gramsPerMole] >>= retainedObject . castPtr

-- | Returns a unique string representation for the unit that could be used with +unitFromString:
--
-- ObjC selector: @- unitString@
unitString :: IsHKUnit hkUnit => hkUnit -> IO (Id NSString)
unitString hkUnit  =
    sendMsg hkUnit (mkSelector "unitString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @unitFromString:@
unitFromStringSelector :: Selector
unitFromStringSelector = mkSelector "unitFromString:"

-- | @Selector@ for @unitFromMassFormatterUnit:@
unitFromMassFormatterUnitSelector :: Selector
unitFromMassFormatterUnitSelector = mkSelector "unitFromMassFormatterUnit:"

-- | @Selector@ for @massFormatterUnitFromUnit:@
massFormatterUnitFromUnitSelector :: Selector
massFormatterUnitFromUnitSelector = mkSelector "massFormatterUnitFromUnit:"

-- | @Selector@ for @unitFromLengthFormatterUnit:@
unitFromLengthFormatterUnitSelector :: Selector
unitFromLengthFormatterUnitSelector = mkSelector "unitFromLengthFormatterUnit:"

-- | @Selector@ for @lengthFormatterUnitFromUnit:@
lengthFormatterUnitFromUnitSelector :: Selector
lengthFormatterUnitFromUnitSelector = mkSelector "lengthFormatterUnitFromUnit:"

-- | @Selector@ for @unitFromEnergyFormatterUnit:@
unitFromEnergyFormatterUnitSelector :: Selector
unitFromEnergyFormatterUnitSelector = mkSelector "unitFromEnergyFormatterUnit:"

-- | @Selector@ for @energyFormatterUnitFromUnit:@
energyFormatterUnitFromUnitSelector :: Selector
energyFormatterUnitFromUnitSelector = mkSelector "energyFormatterUnitFromUnit:"

-- | @Selector@ for @isNull@
isNullSelector :: Selector
isNullSelector = mkSelector "isNull"

-- | @Selector@ for @appleEffortScoreUnit@
appleEffortScoreUnitSelector :: Selector
appleEffortScoreUnitSelector = mkSelector "appleEffortScoreUnit"

-- | @Selector@ for @luxUnitWithMetricPrefix:@
luxUnitWithMetricPrefixSelector :: Selector
luxUnitWithMetricPrefixSelector = mkSelector "luxUnitWithMetricPrefix:"

-- | @Selector@ for @luxUnit@
luxUnitSelector :: Selector
luxUnitSelector = mkSelector "luxUnit"

-- | @Selector@ for @radianAngleUnitWithMetricPrefix:@
radianAngleUnitWithMetricPrefixSelector :: Selector
radianAngleUnitWithMetricPrefixSelector = mkSelector "radianAngleUnitWithMetricPrefix:"

-- | @Selector@ for @radianAngleUnit@
radianAngleUnitSelector :: Selector
radianAngleUnitSelector = mkSelector "radianAngleUnit"

-- | @Selector@ for @degreeAngleUnit@
degreeAngleUnitSelector :: Selector
degreeAngleUnitSelector = mkSelector "degreeAngleUnit"

-- | @Selector@ for @diopterUnit@
diopterUnitSelector :: Selector
diopterUnitSelector = mkSelector "diopterUnit"

-- | @Selector@ for @prismDiopterUnit@
prismDiopterUnitSelector :: Selector
prismDiopterUnitSelector = mkSelector "prismDiopterUnit"

-- | @Selector@ for @wattUnitWithMetricPrefix:@
wattUnitWithMetricPrefixSelector :: Selector
wattUnitWithMetricPrefixSelector = mkSelector "wattUnitWithMetricPrefix:"

-- | @Selector@ for @wattUnit@
wattUnitSelector :: Selector
wattUnitSelector = mkSelector "wattUnit"

-- | @Selector@ for @voltUnitWithMetricPrefix:@
voltUnitWithMetricPrefixSelector :: Selector
voltUnitWithMetricPrefixSelector = mkSelector "voltUnitWithMetricPrefix:"

-- | @Selector@ for @voltUnit@
voltUnitSelector :: Selector
voltUnitSelector = mkSelector "voltUnit"

-- | @Selector@ for @hertzUnitWithMetricPrefix:@
hertzUnitWithMetricPrefixSelector :: Selector
hertzUnitWithMetricPrefixSelector = mkSelector "hertzUnitWithMetricPrefix:"

-- | @Selector@ for @hertzUnit@
hertzUnitSelector :: Selector
hertzUnitSelector = mkSelector "hertzUnit"

-- | @Selector@ for @unitMultipliedByUnit:@
unitMultipliedByUnitSelector :: Selector
unitMultipliedByUnitSelector = mkSelector "unitMultipliedByUnit:"

-- | @Selector@ for @unitDividedByUnit:@
unitDividedByUnitSelector :: Selector
unitDividedByUnitSelector = mkSelector "unitDividedByUnit:"

-- | @Selector@ for @unitRaisedToPower:@
unitRaisedToPowerSelector :: Selector
unitRaisedToPowerSelector = mkSelector "unitRaisedToPower:"

-- | @Selector@ for @reciprocalUnit@
reciprocalUnitSelector :: Selector
reciprocalUnitSelector = mkSelector "reciprocalUnit"

-- | @Selector@ for @decibelHearingLevelUnit@
decibelHearingLevelUnitSelector :: Selector
decibelHearingLevelUnitSelector = mkSelector "decibelHearingLevelUnit"

-- | @Selector@ for @countUnit@
countUnitSelector :: Selector
countUnitSelector = mkSelector "countUnit"

-- | @Selector@ for @percentUnit@
percentUnitSelector :: Selector
percentUnitSelector = mkSelector "percentUnit"

-- | @Selector@ for @internationalUnit@
internationalUnitSelector :: Selector
internationalUnitSelector = mkSelector "internationalUnit"

-- | @Selector@ for @siemenUnitWithMetricPrefix:@
siemenUnitWithMetricPrefixSelector :: Selector
siemenUnitWithMetricPrefixSelector = mkSelector "siemenUnitWithMetricPrefix:"

-- | @Selector@ for @siemenUnit@
siemenUnitSelector :: Selector
siemenUnitSelector = mkSelector "siemenUnit"

-- | @Selector@ for @degreeCelsiusUnit@
degreeCelsiusUnitSelector :: Selector
degreeCelsiusUnitSelector = mkSelector "degreeCelsiusUnit"

-- | @Selector@ for @degreeFahrenheitUnit@
degreeFahrenheitUnitSelector :: Selector
degreeFahrenheitUnitSelector = mkSelector "degreeFahrenheitUnit"

-- | @Selector@ for @kelvinUnit@
kelvinUnitSelector :: Selector
kelvinUnitSelector = mkSelector "kelvinUnit"

-- | @Selector@ for @jouleUnitWithMetricPrefix:@
jouleUnitWithMetricPrefixSelector :: Selector
jouleUnitWithMetricPrefixSelector = mkSelector "jouleUnitWithMetricPrefix:"

-- | @Selector@ for @jouleUnit@
jouleUnitSelector :: Selector
jouleUnitSelector = mkSelector "jouleUnit"

-- | @Selector@ for @kilocalorieUnit@
kilocalorieUnitSelector :: Selector
kilocalorieUnitSelector = mkSelector "kilocalorieUnit"

-- | @Selector@ for @smallCalorieUnit@
smallCalorieUnitSelector :: Selector
smallCalorieUnitSelector = mkSelector "smallCalorieUnit"

-- | @Selector@ for @largeCalorieUnit@
largeCalorieUnitSelector :: Selector
largeCalorieUnitSelector = mkSelector "largeCalorieUnit"

-- | @Selector@ for @calorieUnit@
calorieUnitSelector :: Selector
calorieUnitSelector = mkSelector "calorieUnit"

-- | @Selector@ for @secondUnitWithMetricPrefix:@
secondUnitWithMetricPrefixSelector :: Selector
secondUnitWithMetricPrefixSelector = mkSelector "secondUnitWithMetricPrefix:"

-- | @Selector@ for @secondUnit@
secondUnitSelector :: Selector
secondUnitSelector = mkSelector "secondUnit"

-- | @Selector@ for @minuteUnit@
minuteUnitSelector :: Selector
minuteUnitSelector = mkSelector "minuteUnit"

-- | @Selector@ for @hourUnit@
hourUnitSelector :: Selector
hourUnitSelector = mkSelector "hourUnit"

-- | @Selector@ for @dayUnit@
dayUnitSelector :: Selector
dayUnitSelector = mkSelector "dayUnit"

-- | @Selector@ for @pascalUnitWithMetricPrefix:@
pascalUnitWithMetricPrefixSelector :: Selector
pascalUnitWithMetricPrefixSelector = mkSelector "pascalUnitWithMetricPrefix:"

-- | @Selector@ for @pascalUnit@
pascalUnitSelector :: Selector
pascalUnitSelector = mkSelector "pascalUnit"

-- | @Selector@ for @millimeterOfMercuryUnit@
millimeterOfMercuryUnitSelector :: Selector
millimeterOfMercuryUnitSelector = mkSelector "millimeterOfMercuryUnit"

-- | @Selector@ for @centimeterOfWaterUnit@
centimeterOfWaterUnitSelector :: Selector
centimeterOfWaterUnitSelector = mkSelector "centimeterOfWaterUnit"

-- | @Selector@ for @atmosphereUnit@
atmosphereUnitSelector :: Selector
atmosphereUnitSelector = mkSelector "atmosphereUnit"

-- | @Selector@ for @decibelAWeightedSoundPressureLevelUnit@
decibelAWeightedSoundPressureLevelUnitSelector :: Selector
decibelAWeightedSoundPressureLevelUnitSelector = mkSelector "decibelAWeightedSoundPressureLevelUnit"

-- | @Selector@ for @inchesOfMercuryUnit@
inchesOfMercuryUnitSelector :: Selector
inchesOfMercuryUnitSelector = mkSelector "inchesOfMercuryUnit"

-- | @Selector@ for @literUnitWithMetricPrefix:@
literUnitWithMetricPrefixSelector :: Selector
literUnitWithMetricPrefixSelector = mkSelector "literUnitWithMetricPrefix:"

-- | @Selector@ for @literUnit@
literUnitSelector :: Selector
literUnitSelector = mkSelector "literUnit"

-- | @Selector@ for @fluidOunceUSUnit@
fluidOunceUSUnitSelector :: Selector
fluidOunceUSUnitSelector = mkSelector "fluidOunceUSUnit"

-- | @Selector@ for @fluidOunceImperialUnit@
fluidOunceImperialUnitSelector :: Selector
fluidOunceImperialUnitSelector = mkSelector "fluidOunceImperialUnit"

-- | @Selector@ for @pintUSUnit@
pintUSUnitSelector :: Selector
pintUSUnitSelector = mkSelector "pintUSUnit"

-- | @Selector@ for @pintImperialUnit@
pintImperialUnitSelector :: Selector
pintImperialUnitSelector = mkSelector "pintImperialUnit"

-- | @Selector@ for @cupUSUnit@
cupUSUnitSelector :: Selector
cupUSUnitSelector = mkSelector "cupUSUnit"

-- | @Selector@ for @cupImperialUnit@
cupImperialUnitSelector :: Selector
cupImperialUnitSelector = mkSelector "cupImperialUnit"

-- | @Selector@ for @meterUnitWithMetricPrefix:@
meterUnitWithMetricPrefixSelector :: Selector
meterUnitWithMetricPrefixSelector = mkSelector "meterUnitWithMetricPrefix:"

-- | @Selector@ for @meterUnit@
meterUnitSelector :: Selector
meterUnitSelector = mkSelector "meterUnit"

-- | @Selector@ for @inchUnit@
inchUnitSelector :: Selector
inchUnitSelector = mkSelector "inchUnit"

-- | @Selector@ for @footUnit@
footUnitSelector :: Selector
footUnitSelector = mkSelector "footUnit"

-- | @Selector@ for @yardUnit@
yardUnitSelector :: Selector
yardUnitSelector = mkSelector "yardUnit"

-- | @Selector@ for @mileUnit@
mileUnitSelector :: Selector
mileUnitSelector = mkSelector "mileUnit"

-- | @Selector@ for @gramUnitWithMetricPrefix:@
gramUnitWithMetricPrefixSelector :: Selector
gramUnitWithMetricPrefixSelector = mkSelector "gramUnitWithMetricPrefix:"

-- | @Selector@ for @gramUnit@
gramUnitSelector :: Selector
gramUnitSelector = mkSelector "gramUnit"

-- | @Selector@ for @ounceUnit@
ounceUnitSelector :: Selector
ounceUnitSelector = mkSelector "ounceUnit"

-- | @Selector@ for @poundUnit@
poundUnitSelector :: Selector
poundUnitSelector = mkSelector "poundUnit"

-- | @Selector@ for @stoneUnit@
stoneUnitSelector :: Selector
stoneUnitSelector = mkSelector "stoneUnit"

-- | @Selector@ for @moleUnitWithMetricPrefix:molarMass:@
moleUnitWithMetricPrefix_molarMassSelector :: Selector
moleUnitWithMetricPrefix_molarMassSelector = mkSelector "moleUnitWithMetricPrefix:molarMass:"

-- | @Selector@ for @moleUnitWithMolarMass:@
moleUnitWithMolarMassSelector :: Selector
moleUnitWithMolarMassSelector = mkSelector "moleUnitWithMolarMass:"

-- | @Selector@ for @unitString@
unitStringSelector :: Selector
unitStringSelector = mkSelector "unitString"

