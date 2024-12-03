{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Poseidon.ColumnTypes where

import           Poseidon.ColumnTypesUtils

import           Country                    (Country, alphaTwoUpper,
                                             decodeAlphaTwo)
import qualified Data.Csv                   as Csv
import qualified Data.Text                  as T
import qualified Data.Text.Read             as T
import           GHC.Generics               (Generic)
import           Network.URI                (isURIReference)
import           SequenceFormats.Eigenstrat (Sex (..))
import qualified Text.Regex.TDFA            as Reg

-- | A datatype for the Genetic_Sex .janno column
newtype GeneticSex = GeneticSex { sfSex :: Sex } deriving (Eq)

instance Makeable GeneticSex where
    make x
        | x == "F"  = pure (GeneticSex Female)
        | x == "M"  = pure (GeneticSex Male)
        | x == "U"  = pure (GeneticSex Unknown)
        | otherwise = fail $ "Genetic_Sex is set to " ++ show x ++ ". " ++
                             "That is not in the allowed set [F, M, U]."
instance Show GeneticSex where
    show (GeneticSex Female)  = "F"
    show (GeneticSex Male)    = "M"
    show (GeneticSex Unknown) = "U"
instance Ord GeneticSex where
    compare (GeneticSex Female) (GeneticSex Male)    = GT
    compare (GeneticSex Male) (GeneticSex Female)    = LT
    compare (GeneticSex Male) (GeneticSex Unknown)   = GT
    compare (GeneticSex Unknown) (GeneticSex Male)   = LT
    compare (GeneticSex Female) (GeneticSex Unknown) = GT
    compare (GeneticSex Unknown) (GeneticSex Female) = LT
    compare _ _                                      = EQ
instance Csv.ToField GeneticSex where   toField x  = Csv.toField $ show x
instance Csv.FromField GeneticSex where parseField = parseTypeCSV "Genetic_Sex"

-- | A datatype for the Group_Name .janno column
newtype GroupName = GroupName T.Text deriving (Eq, Ord)
$(makeInstances ''GroupName "Group_Name")

-- | A datatype for the Alternative_IDs .janno column
newtype JannoAlternativeID = JannoAlternativeID T.Text deriving (Eq)
$(makeInstances ''JannoAlternativeID "Alternative_IDs")

-- | A datatype for the Relation_To .janno column
newtype JannoRelationTo = JannoRelationTo T.Text deriving (Eq)
$(makeInstances ''JannoRelationTo "Relation_To")

-- | A datatype for the Relation_Degree .janno column
data JannoRelationDegree =
      Identical
    | First
    | Second
    | ThirdToFifth
    | SixthToTenth
    | Unrelated
    | OtherDegree
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance Makeable JannoRelationDegree where
    make x
        | x == "identical"    = pure Identical
        | x == "first"        = pure First
        | x == "second"       = pure Second
        | x == "thirdToFifth" = pure ThirdToFifth
        | x == "sixthToTenth" = pure SixthToTenth
        | x == "unrelated"    = pure Unrelated -- this should be omitted in the documentation
                                               -- relations of type "unrelated" don't have to be
                                               -- listed explicitly
        | x == "other"        = pure OtherDegree
        | otherwise           = fail $ "Relation_Degree is set to " ++ show x ++ ". " ++
                                       "That is not in the allowed set [identical, first, second, thirdToFifth, sixthToTenth, other]."
instance Show JannoRelationDegree where
    show Identical    = "identical"
    show First        = "first"
    show Second       = "second"
    show ThirdToFifth = "thirdToFifth"
    show SixthToTenth = "sixthToTenth"
    show Unrelated    = "unrelated"
    show OtherDegree  = "other"
instance Csv.ToField JannoRelationDegree where   toField x = Csv.toField $ show x
instance Csv.FromField JannoRelationDegree where parseField = parseTypeCSV "Relation_Degree"

-- | A datatype for the Relation_Type .janno column
newtype JannoRelationType = JannoRelationType T.Text deriving (Eq)
$(makeInstances ''JannoRelationType "Relation_Type")

-- | A datatype for the Relation_Note .janno column
newtype JannoRelationNote = JannoRelationNote T.Text deriving (Eq)
$(makeInstances ''JannoRelationNote "Relation_Note")

-- | A datatype for the Collection_ID .janno column
newtype JannoCollectionID = JannoCollectionID T.Text deriving (Eq)
$(makeInstances ''JannoCollectionID "Collection_ID")

-- | A datatype for the Country .janno column
newtype JannoCountry = JannoCountry T.Text deriving (Eq, Ord)
$(makeInstances ''JannoCountry "Country")

-- | A datatype for countries in ISO-alpha2 code format
newtype JannoCountryISO = JannoCountryISO Country deriving (Eq, Ord)

instance Show JannoCountryISO where
    show (JannoCountryISO x) = T.unpack $ alphaTwoUpper x
instance Makeable JannoCountryISO where
    make x = case decodeAlphaTwo x of
        Nothing -> fail $
            "Country_ISO is set to " ++ show x ++ ". " ++
            "That is not a valid ISO-alpha2 code describing an existing country."
        Just c  -> return $ JannoCountryISO c
instance Csv.ToField JannoCountryISO where   toField x = Csv.toField $ show x
instance Csv.FromField JannoCountryISO where parseField = parseTypeCSV "Country_ISO"

-- | A datatype for the Location .janno column
newtype JannoLocation = JannoLocation T.Text deriving (Eq)
$(makeInstances ''JannoLocation "Location")

-- | A datatype for the Site .janno column
newtype JannoSite = JannoSite T.Text deriving (Eq, Ord)
$(makeInstances ''JannoSite "Site")

-- | A datatype for the Latitude .janno column
newtype JannoLatitude = JannoLatitude Double deriving (Eq, Ord, Generic)

instance Makeable JannoLatitude where
    make x =
        case T.double x of
            Left e -> fail $ "Latitude can not be converted to Double because " ++ e
            Right (num, "") ->
                if num >= -90 && num <= 90
                then pure (JannoLatitude num)
                else fail $ "Latitude " ++ show x ++ " not between -90 and 90"
            Right (_, rest) -> fail $ "Latitude can not be converted to Double, because of a trailing " ++ show rest
instance Show JannoLatitude where          show (JannoLatitude x) = show x
instance Csv.ToField JannoLatitude where   toField (JannoLatitude x) = Csv.toField x
instance Csv.FromField JannoLatitude where parseField = parseTypeCSV "Latitude"

-- | A datatype for the Longitude .janno column
newtype JannoLongitude = JannoLongitude Double deriving (Eq, Ord, Generic)

instance Makeable JannoLongitude where
    make x =
        case T.double x of
            Left e -> fail $ "Longitude can not be converted to Double because " ++ e
            Right (num, "") ->
                if num >= -180 && num <= 180
                then pure (JannoLongitude num)
                else fail $ "Longitude " ++ show x ++ " not between -180 and 180"
            Right (_, rest) -> fail $ "Longitude can not be converted to Double, because of a trailing " ++ show rest
instance Show JannoLongitude where          show (JannoLongitude x) = show x
instance Csv.ToField JannoLongitude where   toField (JannoLongitude x) = Csv.toField x
instance Csv.FromField JannoLongitude where parseField = parseTypeCSV "Longitude"

-- | A datatype for the Date_Type .janno column
data JannoDateType =
      C14
    | Contextual
    | Modern
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance Makeable JannoDateType where
    make x
        | x == "C14"        = pure C14
        | x == "contextual" = pure Contextual
        | x == "modern"     = pure Modern
        | otherwise         = fail $ "Date_Type is set to " ++ show x ++ ". " ++
                                     "That is not in the allowed set [C14, contextual, modern]."
instance Show JannoDateType where
    show C14        = "C14"
    show Contextual = "contextual"
    show Modern     = "modern"
instance Csv.ToField JannoDateType where   toField x = Csv.toField $ show x
instance Csv.FromField JannoDateType where parseField = parseTypeCSV "Date_Type"

-- | A datatype for the Date_C14_Labnr .janno column
newtype JannoDateC14Labnr = JannoDateC14Labnr T.Text deriving (Eq)
$(makeInstances ''JannoDateC14Labnr "Date_C14_Labnr")

-- | A datatype for the Date_C14_Uncal_BP .janno column
newtype JannoDateC14UncalBP = JannoDateC14UncalBP Int deriving (Eq, Ord, Generic)

instance Makeable JannoDateC14UncalBP where
    make x =
        case T.decimal x of
            Left e -> fail $ "Date_C14_Uncal_BP can not be converted to Int because " ++ e
            Right (num, "") -> pure $ JannoDateC14UncalBP num
            Right (_, rest) -> fail $ "Date_C14_Uncal_BP can not be converted to Int, because of a trailing " ++ show rest
instance Show JannoDateC14UncalBP where          show (JannoDateC14UncalBP x) = show x
instance Csv.ToField JannoDateC14UncalBP where   toField (JannoDateC14UncalBP x) = Csv.toField x
instance Csv.FromField JannoDateC14UncalBP where parseField = parseTypeCSV "Date_C14_Uncal_BP"

-- | A datatype for the Date_C14_Uncal_BP_Err .janno column
newtype JannoDateC14UncalBPErr = JannoDateC14UncalBPErr Int deriving (Eq, Ord, Generic)

instance Makeable JannoDateC14UncalBPErr where
    make x =
        case T.decimal x of
            Left e -> fail $ "Date_C14_Uncal_BP_Err can not be converted to Int because " ++ e
            Right (num, "") -> pure $ JannoDateC14UncalBPErr num
            Right (_, rest) -> fail $ "Date_C14_Uncal_BP_Err can not be converted to Int, because of a trailing " ++ show rest
instance Show JannoDateC14UncalBPErr where          show (JannoDateC14UncalBPErr x) = show x
instance Csv.ToField JannoDateC14UncalBPErr where   toField (JannoDateC14UncalBPErr x) = Csv.toField x
instance Csv.FromField JannoDateC14UncalBPErr where parseField = parseTypeCSV "Date_C14_Uncal_BP_Err"

-- | A datatype for the Date_BC_AD_Start .janno column
newtype JannoDateBCADStart = JannoDateBCADStart Int deriving (Eq, Ord, Generic)

instance Makeable JannoDateBCADStart where
    make x =
        let curYear = 2024 -- the current year
        in case T.signed T.decimal x of
            Left e -> fail $ "Date_BC_AD_Start can not be converted to Int because " ++ e
            Right (num, "") ->
                if num >= curYear
                then fail $ "Date_BC_AD_Start " ++ show x ++ " later than " ++ show curYear ++ ", which is impossible. " ++
                   "Did you accidentally enter a BP date?"
                else pure $ JannoDateBCADStart num
            Right (_, rest) -> fail $ "Date_BC_AD_Start can not be converted to Int, because of a trailing " ++ show rest
instance Show JannoDateBCADStart where          show (JannoDateBCADStart x) = show x
instance Csv.ToField JannoDateBCADStart where   toField (JannoDateBCADStart x) = Csv.toField x
instance Csv.FromField JannoDateBCADStart where parseField = parseTypeCSV "Date_BC_AD_Start"

-- | A datatype for the Date_BC_AD_Median .janno column
newtype JannoDateBCADMedian = JannoDateBCADMedian Int deriving (Eq, Ord, Generic)

instance Makeable JannoDateBCADMedian where
    make x =
        let curYear = 2024 -- the current year
        in case T.signed T.decimal x of
            Left e -> fail $ "Date_BC_AD_Median can not be converted to Int because " ++ e
            Right (num, "") ->
                if num >= curYear
                then fail $ "Date_BC_AD_Median " ++ show x ++ " later than " ++ show curYear ++ ", which is impossible. " ++
                   "Did you accidentally enter a BP date?"
                else pure $ JannoDateBCADMedian num
            Right (_, rest) -> fail $ "Date_BC_AD_Median can not be converted to Int, because of a trailing " ++ show rest
instance Show JannoDateBCADMedian where          show (JannoDateBCADMedian x) = show x
instance Csv.ToField JannoDateBCADMedian where   toField (JannoDateBCADMedian x) = Csv.toField x
instance Csv.FromField JannoDateBCADMedian where parseField = parseTypeCSV "Date_BC_AD_Median"

-- | A datatype for the Date_BC_AD_Stop .janno column
newtype JannoDateBCADStop = JannoDateBCADStop Int deriving (Eq, Ord, Generic)

instance Makeable JannoDateBCADStop where
    make x =
        let curYear = 2024 -- the current year
        in case T.signed T.decimal x of
            Left e -> fail $ "Date_BC_AD_Stop can not be converted to Int because " ++ e
            Right (num, "") ->
                if num >= curYear
                then fail $ "Date_BC_AD_Stop " ++ show x ++ " later than " ++ show curYear ++ ", which is impossible. " ++
                   "Did you accidentally enter a BP date?"
                else pure $ JannoDateBCADStop num
            Right (_, rest) -> fail $ "Date_BC_AD_Stop can not be converted to Int, because of a trailing " ++ show rest
instance Show JannoDateBCADStop where          show (JannoDateBCADStop x) = show x
instance Csv.ToField JannoDateBCADStop where   toField (JannoDateBCADStop x) = Csv.toField x
instance Csv.FromField JannoDateBCADStop where parseField = parseTypeCSV "Date_BC_AD_Stop"

-- | A datatype for the Date_Note .janno column
newtype JannoDateNote = JannoDateNote T.Text deriving (Eq, Ord)
$(makeInstances ''JannoDateNote "Date_Note")

-- | A datatype for the MT_Haplogroup .janno column
newtype JannoMTHaplogroup = JannoMTHaplogroup T.Text deriving (Eq, Ord)
$(makeInstances ''JannoMTHaplogroup "MT_Haplogroup")

-- | A datatype for the Y_Haplogroup .janno column
newtype JannoYHaplogroup = JannoYHaplogroup T.Text deriving (Eq, Ord)
$(makeInstances ''JannoYHaplogroup "Y_Haplogroup")

-- | A datatype for the Source_Tissue .janno column
newtype JannoSourceTissue = JannoSourceTissue T.Text deriving (Eq)
$(makeInstances ''JannoSourceTissue "Source_Tissue")

-- | A datatype for the Nr_Libraries .janno column
newtype JannoNrLibraries = JannoNrLibraries Int deriving (Eq, Ord, Generic)

instance Makeable JannoNrLibraries where
    make x =
        case T.signed T.decimal x of
            Left e -> fail $ "Nr_Libraries can not be converted to Int because " ++ e
            Right (num, "") ->
                if num < 1
                then fail $ "Nr_Libraries " ++ show x ++ " lower than 1, which is impossible."
                else pure $ JannoNrLibraries num
            Right (_, rest) -> fail $ "Nr_Libraries can not be converted to Int, because of a trailing " ++ show rest
instance Show JannoNrLibraries where          show (JannoNrLibraries x) = show x
instance Csv.ToField JannoNrLibraries where   toField (JannoNrLibraries x) = Csv.toField x
instance Csv.FromField JannoNrLibraries where parseField = parseTypeCSV "Nr_Libraries"

-- | A datatype for the Library_Names .janno column
newtype JannoLibraryName = JannoLibraryName T.Text deriving (Eq)
$(makeInstances ''JannoLibraryName "Library_Names")

-- | A datatype for the Capture_Type .janno column
data JannoCaptureType =
      Shotgun
    | A1240K
    | ArborComplete
    | ArborPrimePlus
    | ArborAncestralPlus
    | TwistAncientDNA
    | OtherCapture
    | ReferenceGenome
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance Makeable JannoCaptureType where
    make x
        | x == "Shotgun"            = pure Shotgun
        | x == "1240K"              = pure A1240K
        | x == "ArborComplete"      = pure ArborComplete
        | x == "ArborPrimePlus"     = pure ArborPrimePlus
        | x == "ArborAncestralPlus" = pure ArborAncestralPlus
        | x == "TwistAncientDNA"    = pure TwistAncientDNA
        | x == "OtherCapture"       = pure OtherCapture
        | x == "ReferenceGenome"    = pure ReferenceGenome
        | otherwise = fail $ "Capture_Type is set to " ++ show x ++ ". " ++
                             "That is not in the allowed set [Shotgun, 1240K, ArborComplete, ArborPrimePlus, ArborAncestralPlus, TwistAncientDNA, OtherCapture, ReferenceGenome]."
instance Show JannoCaptureType where
    show Shotgun            = "Shotgun"
    show A1240K             = "1240K"
    show ArborComplete      = "ArborComplete"
    show ArborPrimePlus     = "ArborPrimePlus"
    show ArborAncestralPlus = "ArborAncestralPlus"
    show TwistAncientDNA    = "TwistAncientDNA"
    show OtherCapture       = "OtherCapture"
    show ReferenceGenome    = "ReferenceGenome"
instance Csv.ToField JannoCaptureType where   toField x = Csv.toField $ show x
instance Csv.FromField JannoCaptureType where parseField = parseTypeCSV "Capture_Type"

-- | A datatype for the UDG .janno column
data JannoUDG =
      Minus
    | Half
    | Plus
    | Mixed
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance Makeable JannoUDG where
    make x
        | x == "minus" = pure Minus
        | x == "half"  = pure Half
        | x == "plus"  = pure Plus
        | x == "mixed" = pure Mixed
        | otherwise    = fail $ "UDG is set to " ++ show x ++ ". " ++
                                "That is not in the allowed set [minus, half, plus, mixed]."
instance Show JannoUDG where
    show Minus = "minus"
    show Half  = "half"
    show Plus  = "plus"
    show Mixed = "mixed"
instance Csv.ToField JannoUDG where   toField x = Csv.toField $ show x
instance Csv.FromField JannoUDG where parseField = parseTypeCSV "UDG"

-- | A datatype for the Library_Built .janno column
data JannoLibraryBuilt =
      DS
    | SS
    | MixedSSDS
    | Other -- the "other" option is deprecated and should be removed at some point
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance Makeable JannoLibraryBuilt where
    make x
        | x == "ds"    = pure DS
        | x == "ss"    = pure SS
        | x == "mixed" = pure MixedSSDS
        | x == "other" = pure Other
        | otherwise    = fail $ "Library_Built is set to " ++ show x ++ ". " ++
                                "That is not in the allowed set [ds, ss, mixed]."
instance Show JannoLibraryBuilt where
    show DS        = "ds"
    show SS        = "ss"
    show MixedSSDS = "mixed"
    show Other     = "other"
instance Csv.ToField JannoLibraryBuilt where   toField x = Csv.toField $ show x
instance Csv.FromField JannoLibraryBuilt where parseField = parseTypeCSV "Library_Built"

-- | A datatype for the Genotype_Ploidy .janno column
data JannoGenotypePloidy =
      Diploid
    | Haploid
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance Makeable JannoGenotypePloidy where
    make x
        | x == "diploid" = pure Diploid
        | x == "haploid" = pure Haploid
        | otherwise      = fail $ "Genotype_Ploidy is set to " ++ show x ++ ". " ++
                                  "That is not in the allowed set [diploid, haploid]."
instance Show JannoGenotypePloidy where
    show Diploid = "diploid"
    show Haploid = "haploid"
instance Csv.ToField JannoGenotypePloidy where   toField x = Csv.toField $ show x
instance Csv.FromField JannoGenotypePloidy where parseField = parseTypeCSV "Genotype_Ploidy"

-- | A datatype for the Genotype_Ploidy .janno column
newtype JannoDataPreparationPipelineURL = JannoDataPreparationPipelineURL T.Text deriving (Eq, Ord, Generic)

instance Makeable JannoDataPreparationPipelineURL where
    make x
        | isURIReference (T.unpack x) = pure $ JannoDataPreparationPipelineURL x
        | otherwise                   = fail $ "Data_Preparation_Pipeline_URL " ++ show x ++ " is not a well structured URI."
instance Show JannoDataPreparationPipelineURL where          show (JannoDataPreparationPipelineURL x) = T.unpack x
instance Csv.ToField JannoDataPreparationPipelineURL where   toField (JannoDataPreparationPipelineURL x) = Csv.toField x
instance Csv.FromField JannoDataPreparationPipelineURL where parseField = parseTypeCSV "Data_Preparation_Pipeline_URL"

-- | A datatype for the Endogenous .janno column
newtype JannoEndogenous = JannoEndogenous Double deriving (Eq, Ord, Generic)

instance Makeable JannoEndogenous where
    make x =
        case T.double x of
            Left e -> fail $ "Endogenous can not be converted to Double because " ++ e
            Right (num, "") ->
                if num >= 0 && num <= 100
                then pure (JannoEndogenous num)
                else fail $ "Endogenous " ++ show x ++ " not between 0 and 100."
            Right (_, rest) -> fail $ "Endogenous can not be converted to Double, because of a trailing " ++ show rest
instance Show JannoEndogenous where          show (JannoEndogenous x) = show x
instance Csv.ToField JannoEndogenous where   toField (JannoEndogenous x) = Csv.toField x
instance Csv.FromField JannoEndogenous where parseField = parseTypeCSV "Endogenous"

-- | A datatype for the Nr_SNPs .janno column
newtype JannoNrSNPs = JannoNrSNPs Int deriving (Eq, Ord, Generic)

instance Makeable JannoNrSNPs where
    make x =
        case T.signed T.decimal x of
            Left e -> fail $ "Nr_SNPs can not be converted to Int because " ++ e
            Right (num, "") ->
                if num < 0
                then fail $ "Nr_SNPs " ++ show x ++ " lower than 0, which is not meaningful."
                else pure $ JannoNrSNPs num
            Right (_, rest) -> fail $ "Nr_SNPs can not be converted to Int, because of a trailing " ++ show rest
instance Show JannoNrSNPs where          show (JannoNrSNPs x) = show x
instance Csv.ToField JannoNrSNPs where   toField (JannoNrSNPs x) = Csv.toField x
instance Csv.FromField JannoNrSNPs where parseField = parseTypeCSV "Nr_SNPs"

-- | A datatype for the Coverage_on_Target_SNPs .janno column
newtype JannoCoverageOnTargets = JannoCoverageOnTargets Double deriving (Eq, Ord, Generic)

instance Makeable JannoCoverageOnTargets where
    make x =
        case T.double x of
            Left e -> fail $ "Coverage_on_Target_SNPs can not be converted to Double because " ++ e
            Right (num, "") ->  pure (JannoCoverageOnTargets num)
            Right (_, rest) -> fail $ "Coverage_on_Target_SNPs can not be converted to Double, because of a trailing " ++ show rest
instance Show JannoCoverageOnTargets where          show (JannoCoverageOnTargets x) = show x
instance Csv.ToField JannoCoverageOnTargets where   toField (JannoCoverageOnTargets x) = Csv.toField x
instance Csv.FromField JannoCoverageOnTargets where parseField = parseTypeCSV "Coverage_on_Target_SNPs"

-- | A datatype for the Damage .janno column
newtype JannoDamage = JannoDamage Double deriving (Eq, Ord, Generic)

instance Makeable JannoDamage where
    make x =
        case T.double x of
            Left e -> fail $ "Damage can not be converted to Double because " ++ e
            Right (num, "") ->
                if num >= 0 && num <= 100
                then pure (JannoDamage num)
                else fail $ "Damage " ++ show x ++ " not between 0 and 100."
            Right (_, rest) -> fail $ "Damage can not be converted to Double, because of a trailing " ++ show rest
instance Show JannoDamage where          show (JannoDamage x) = show x
instance Csv.ToField JannoDamage where   toField (JannoDamage x) = Csv.toField x
instance Csv.FromField JannoDamage where parseField = parseTypeCSV "Damage"

-- | A datatype for the Contamination .janno column
newtype JannoContamination = JannoContamination T.Text deriving (Eq)
$(makeInstances ''JannoContamination "Contamination")

-- | A datatype for the Contamination_Err .janno column
newtype JannoContaminationErr = JannoContaminationErr T.Text deriving (Eq)
$(makeInstances ''JannoContaminationErr "Contamination_Err")

-- | A datatype for the Contamination_Meas .janno column
newtype JannoContaminationMeas = JannoContaminationMeas T.Text deriving (Eq)
$(makeInstances ''JannoContaminationMeas "Contamination_Meas")

-- | A datatype for the Contamination_Note .janno column
newtype JannoContaminationNote = JannoContaminationNote T.Text deriving (Eq)
$(makeInstances ''JannoContaminationNote "Contamination_Note")

-- | A datatype for the Genetic_Source_Accession_IDs .janno column
data JannoGeneticSourceAccessionID =
      INSDCProject T.Text
    | INSDCStudy T.Text
    | INSDCBioSample T.Text
    | INSDCSample T.Text
    | INSDCExperiment T.Text
    | INSDCRun T.Text
    | INSDCAnalysis T.Text
    | OtherID T.Text
    deriving (Eq, Ord, Generic)

instance Makeable JannoGeneticSourceAccessionID where
    make x
        | (T.unpack x) Reg.=~ ("PRJ[EDN][A-Z][0-9]+"  :: String) = pure $ INSDCProject x
        | (T.unpack x) Reg.=~ ("[EDS]RP[0-9]{6,}"     :: String) = pure $ INSDCStudy x
        | (T.unpack x) Reg.=~ ("SAM[EDN][A-Z]?[0-9]+" :: String) = pure $ INSDCBioSample x
        | (T.unpack x) Reg.=~ ("[EDS]RS[0-9]{6,}"     :: String) = pure $ INSDCSample x
        | (T.unpack x) Reg.=~ ("[EDS]RX[0-9]{6,}"     :: String) = pure $ INSDCExperiment x
        | (T.unpack x) Reg.=~ ("[EDS]RR[0-9]{6,}"     :: String) = pure $ INSDCRun x
        | (T.unpack x) Reg.=~ ("[EDS]RZ[0-9]{6,}"     :: String) = pure $ INSDCAnalysis x
        | otherwise                                   = pure $ OtherID x
instance Show JannoGeneticSourceAccessionID where
    show (INSDCProject x)    = T.unpack x
    show (INSDCStudy x)      = T.unpack x
    show (INSDCBioSample x)  = T.unpack x
    show (INSDCSample x)     = T.unpack x
    show (INSDCExperiment x) = T.unpack x
    show (INSDCRun x)        = T.unpack x
    show (INSDCAnalysis x)   = T.unpack x
    show (OtherID x)         = T.unpack x
instance Csv.ToField JannoGeneticSourceAccessionID where   toField x  = Csv.toField $ show x
instance Csv.FromField JannoGeneticSourceAccessionID where parseField = parseTypeCSV "Genetic_Source_Accession_IDs"

-- | A datatype for the Primary_Contact .janno column
newtype JannoPrimaryContact = JannoPrimaryContact T.Text deriving (Eq)
$(makeInstances ''JannoPrimaryContact "Primary_Contact")

-- | A datatype for the Publication .janno column
newtype JannoPublication = JannoPublication T.Text deriving (Eq, Ord)
$(makeInstances ''JannoPublication "Publication")

-- | A datatype for the Note .janno column
newtype JannoComment = JannoComment T.Text deriving (Eq)
$(makeInstances ''JannoComment "Note")

-- | A datatype for the Keywords .janno column
newtype JannoKeyword = JannoKeyword T.Text deriving (Eq)
$(makeInstances ''JannoKeyword "Keywords")
