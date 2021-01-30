{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poseidon.Janno (
    Janno (..),
    PoseidonSample(..),
    Sex (..),
    Latitude (..),
    Longitude (..),
    JannoDateType (..),
    JannoDataType (..),
    JannoGenotypePloidy (..),
    Percent (..),
    JannoUDG (..),
    JannoLibraryBuilt (..),
    jannoToSimpleMaybeList,
    writeJannoFile,
    loadJannoFile,
    createMinimalSamplesList
) where

import           Poseidon.Utils             (PoseidonException (..), renderPoseidonException)

import           Control.Applicative        (empty)
import           Control.Exception          (throwIO, try)
import           Control.Monad              (when, unless)
import qualified Data.ByteString.Char8      as Bchs
import qualified Data.ByteString.Lazy.Char8 as Bch
import           Data.Char                  (ord)
import qualified Data.Csv                   as Csv
import           Data.Either                (isRight, isLeft, rights, lefts)
import           Data.Either.Combinators    (rightToMaybe)
import           Data.List                  (intercalate, nub)
import qualified Data.Vector                as V
import           GHC.Generics               (Generic)
import           SequenceFormats.Eigenstrat (Sex (..),
                                             EigenstratIndEntry (..))
import           System.Directory           (doesFileExist)
import           System.IO                  (hPutStrLn, stderr)

instance Ord Sex where
    compare Female Male = GT
    compare Male Female = LT
    compare Male Unknown = GT
    compare Unknown Male = LT
    compare Female Unknown = GT
    compare Unknown Female = LT
    compare _ _ = EQ

instance Csv.FromField Sex where
    parseField x
        | x == "F" = pure Female
        | x == "M" = pure Male
        | x == "U" = pure Unknown
        | otherwise = empty

instance Csv.ToField Sex where
    toField Female  = "F"
    toField Male    = "M"
    toField Unknown = "U"

-- |A datatype to represent Date_Type in a janno file
data JannoDateType = C14
    | Contextual
    | Modern
    deriving (Eq, Show, Ord)

instance Csv.FromField JannoDateType where
    parseField x
        | x == "C14" = pure C14
        | x == "contextual" = pure Contextual
        | x == "modern" = pure Modern
        | otherwise = empty

instance Csv.ToField JannoDateType where
    toField C14        = "C14"
    toField Contextual = "contextual"
    toField Modern     = "modern"

-- |A datatype to represent Data_Type in a janno file
data JannoDataType = Shotgun
    | A1240K
    | OtherCapture
    | ReferenceGenome
    deriving (Eq, Show, Ord)

instance Csv.FromField JannoDataType where
    parseField x
        | x == "Shotgun" = pure Shotgun
        | x == "1240K" = pure A1240K
        | x == "OtherCapture" = pure OtherCapture
        | x == "ReferenceGenome" = pure ReferenceGenome
        | otherwise = empty

instance Csv.ToField JannoDataType where
    toField Shotgun         = "Shotgun"
    toField A1240K          = "1240K"
    toField OtherCapture    = "OtherCapture"
    toField ReferenceGenome = "ReferenceGenome"

-- |A datatype to represent Genotype_Ploidy in a janno file
data JannoGenotypePloidy = Diploid
    | Haploid
    deriving (Eq, Show, Ord)

instance Csv.FromField JannoGenotypePloidy where
    parseField x
        | x == "diploid" = pure Diploid
        | x == "haploid" = pure Haploid
        | otherwise = empty

instance Csv.ToField JannoGenotypePloidy where
    toField Diploid = "diploid"
    toField Haploid = "haploid"

-- |A datatype to represent UDG in a janno file
data JannoUDG = Minus
    | Half
    | Plus
    | Mixed
    deriving (Eq, Show, Ord)

instance Csv.FromField JannoUDG where
    parseField x
        | x == "minus" = pure Minus
        | x == "half" = pure Half
        | x == "plus" = pure Plus
        | x == "mixed" = pure Mixed
        | otherwise = empty

instance Csv.ToField JannoUDG where
    toField Minus = "minus"
    toField Half  = "half"
    toField Plus  = "plus"
    toField Mixed = "mixed"

-- |A datatype to represent Library_Built in a janno file
data JannoLibraryBuilt = DS
    | SS
    | Other
    deriving (Eq, Show, Ord)

instance Csv.FromField JannoLibraryBuilt where
    parseField x
        | x == "ds" = pure DS
        | x == "ss" = pure SS
        | x == "other" = pure Other
        | otherwise = empty

instance Csv.ToField JannoLibraryBuilt where
    toField DS    = "ds"
    toField SS    = "ss"
    toField Other = "other"

-- | A datatype for Latitudes
newtype Latitude =
        Latitude Double
    deriving (Eq, Show, Ord)

instance Csv.FromField Latitude where
    parseField x = do
        val <- Csv.parseField x
        if val < -90 || val > 90
        then empty
        else pure (Latitude val)

instance Csv.ToField Latitude where
    toField (Latitude x) = Csv.toField x

-- | A datatype for Longitudes
newtype Longitude =
        Longitude Double
    deriving (Eq, Show, Ord)

instance Csv.FromField Longitude where
    parseField x = do
        val <- Csv.parseField x
        if val < -180 || val > 180
        then empty
        else pure (Longitude val)

instance Csv.ToField Longitude where
    toField (Longitude x) = Csv.toField x

-- | A datatype for Percent values
newtype Percent =
        Percent Double
    deriving (Eq, Show, Ord)

instance Csv.FromField Percent where
    parseField x = do
        val <- Csv.parseField x
        if val < 0  || val > 100
        then empty
        else pure (Percent val)

instance Csv.ToField Percent where
    toField (Percent x) = Csv.toField x

type Janno = [PoseidonSample]

-- | A data type to represent a sample/janno file row
-- See https://github.com/poseidon-framework/poseidon2-schema/blob/master/janno_columns.tsv
-- for more details
data PoseidonSample = PoseidonSample
    { posSamIndividualID      :: String
    , posSamCollectionID      :: Maybe String
    , posSamSourceTissue      :: Maybe [String]
    , posSamCountry           :: Maybe String
    , posSamLocation          :: Maybe String
    , posSamSite              :: Maybe String
    , posSamLatitude          :: Maybe Latitude
    , posSamLongitude         :: Maybe Longitude
    , posSamDateC14Labnr      :: Maybe [String]
    , posSamDateC14UncalBP    :: Maybe [Int]
    , posSamDateC14UncalBPErr :: Maybe [Int]
    , posSamDateBCADMedian    :: Maybe Int
    , posSamDateBCADStart     :: Maybe Int
    , posSamDateBCADStop      :: Maybe Int
    , posSamDateType          :: Maybe JannoDateType
    , posSamNrLibraries       :: Maybe Int
    , posSamDataType          :: Maybe [JannoDataType]
    , posSamGenotypePloidy    :: Maybe JannoGenotypePloidy
    , posSamGroupName         :: [String]
    , posSamGeneticSex        :: Sex
    , posSamNrAutosomalSNPs   :: Maybe Int
    , posSamCoverage1240K     :: Maybe Double
    , posSamMTHaplogroup      :: Maybe String
    , posSamYHaplogroup       :: Maybe String
    , posSamEndogenous        :: Maybe Percent
    , posSamUDG               :: Maybe JannoUDG
    , posSamLibraryBuilt      :: Maybe JannoLibraryBuilt
    , posSamDamage            :: Maybe Percent
    , posSamNuclearContam     :: Maybe Double
    , posSamNuclearContamErr  :: Maybe Double
    , posSamMTContam          :: Maybe Double
    , posSamMTContamErr       :: Maybe Double
    , posSamPrimaryContact    :: Maybe String
    , posSamPublication       :: Maybe String
    , posSamComments          :: Maybe String
    , posSamKeywords          :: Maybe [String]
    }
    deriving (Show, Eq, Generic)

instance Csv.FromRecord PoseidonSample
instance Csv.ToRecord PoseidonSample

-- | A helper function to create semi-colon separated field values from lists
deparseFieldList :: (Csv.ToField a) => [a] -> Csv.Field
deparseFieldList xs = do
    Csv.toField $ intercalate ";" $ map (read . show . Csv.toField) xs

instance Csv.ToField [String] where
    toField = deparseFieldList

instance Csv.ToField [Int] where
    toField = deparseFieldList

instance Csv.ToField [JannoDataType] where
    toField = deparseFieldList

-- | A helper function to parse semi-colon separated field values into lists
parseFieldList :: (Csv.FromField a) => Csv.Field -> Csv.Parser [a]
parseFieldList x = do
    fieldStr <- Csv.parseField x
    let subStrings = Bchs.splitWith (==';') fieldStr
    mapM Csv.parseField subStrings

instance Csv.FromField [String] where
    parseField = parseFieldList

instance Csv.FromField [Int] where
    parseField = parseFieldList

instance Csv.FromField [JannoDataType] where
    parseField = parseFieldList

-- Janno file writing

writeJannoFile :: FilePath -> [PoseidonSample] -> IO ()
writeJannoFile path samples = do
    let jannoHeaderLine = jannoHeader `Bch.append` "\n"
    let jannoAsBytestring = jannoHeaderLine `Bch.append` Csv.encodeWith encodingOptions samples
    let jannoAsBytestringwithNA = explicitNA jannoAsBytestring
    Bch.writeFile path jannoAsBytestringwithNA

jannoHeader :: Bch.ByteString
jannoHeader = Bch.intercalate "\t" ["Individual_ID","Collection_ID","Source_Tissue","Country",
    "Location","Site","Latitude","Longitude","Date_C14_Labnr",
    "Date_C14_Uncal_BP","Date_C14_Uncal_BP_Err","Date_BC_AD_Median","Date_BC_AD_Start",
    "Date_BC_AD_Stop","Date_Type","No_of_Libraries","Data_Type","Genotype_Ploidy","Group_Name",
    "Genetic_Sex","Nr_autosomal_SNPs","Coverage_1240K","MT_Haplogroup","Y_Haplogroup",
    "Endogenous","UDG","Library_Built","Damage","Xcontam","Xcontam_stderr","mtContam",
    "mtContam_stderr","Primary_Contact","Publication_Status","Note","Keywords"
    ]

encodingOptions :: Csv.EncodeOptions
encodingOptions = Csv.defaultEncodeOptions {
      Csv.encDelimiter = fromIntegral (ord '\t')
    , Csv.encIncludeHeader = True
    , Csv.encQuoting = Csv.QuoteMinimal
}

-- | A function to load one janno file
loadJannoFile :: FilePath -> IO [PoseidonSample]
loadJannoFile jannoPath = do
    jannoFile <- Bch.readFile jannoPath
    let jannoFileUpdated = replaceNA jannoFile
    let jannoFileRows = Bch.lines jannoFileUpdated
    -- tupel with row number and row bytestring
    let jannoFileRowsWithNumber = zip [1..(length jannoFileRows)] jannoFileRows
    -- load janno by rows
    jannoRepresentation <- mapM (loadJannoFileRow jannoPath) (tail jannoFileRowsWithNumber)
    -- error case management
    if not (null (lefts jannoRepresentation))
    then do
        mapM_ (hPutStrLn stderr . renderPoseidonException) $ take 5 $ lefts jannoRepresentation
        throwIO $ PoseidonJannoConsistencyException jannoPath "Broken lines"
    else do
        let consistentJanno = checkJannoConsistency jannoPath $ rights jannoRepresentation
        case consistentJanno of
            Left e -> do throwIO e
            Right x -> return x

-- | A function to load one row of a janno file
loadJannoFileRow :: FilePath -> (Int, Bch.ByteString) -> IO (Either PoseidonException PoseidonSample)
loadJannoFileRow jannoPath row = do
    case Csv.decodeWith decodingOptions Csv.NoHeader (snd row) of
        Left _ -> do 
            return $ Left $ PoseidonJannoRowException jannoPath (fst row) "Type error"
        Right (poseidonSample :: V.Vector PoseidonSample) -> do
            case checkJannoRowConsistency jannoPath (fst row) $ V.head poseidonSample of
                Left e -> do
                    return $ Left e
                Right (pS :: PoseidonSample) -> do
                    return $ Right pS

decodingOptions :: Csv.DecodeOptions
decodingOptions = Csv.defaultDecodeOptions {
    Csv.decDelimiter = fromIntegral (ord '\t')
}

-- | A helper functions to replace n/a values in janno files with
replaceNA :: Bch.ByteString -> Bch.ByteString
replaceNA = replaceInJannoBytestring "n/a" Bch.empty

-- | A helper functions to replace empty bytestrings values in janno files with explicit "n/a"
explicitNA :: Bch.ByteString -> Bch.ByteString
explicitNA = replaceInJannoBytestring Bch.empty "n/a"

replaceInJannoBytestring :: Bch.ByteString -> Bch.ByteString -> Bch.ByteString -> Bch.ByteString
replaceInJannoBytestring from to tsv =
    let tsvRows = Bch.lines tsv
        tsvCells = map (Bch.splitWith (=='\t')) tsvRows
        tsvCellsUpdated = map (map (\y -> if y == from || y == Bch.append from "\r" then to else y)) tsvCells
        tsvRowsUpdated = map (Bch.intercalate (Bch.pack "\t")) tsvCellsUpdated
   in Bch.unlines tsvRowsUpdated

jannoToSimpleMaybeList :: [Either PoseidonException [Either PoseidonException PoseidonSample]] -> [Maybe [PoseidonSample]]
jannoToSimpleMaybeList = map (maybe Nothing (\x -> if all isRight x then Just (rights x) else Nothing) . rightToMaybe)

-- | A function to create empty janno rows for a set of individuals
createMinimalSamplesList :: [EigenstratIndEntry] -> [PoseidonSample]
createMinimalSamplesList = map createMinimalSample 

-- | A function to create an empty janno row for an individual
createMinimalSample :: EigenstratIndEntry -> PoseidonSample
createMinimalSample (EigenstratIndEntry id sex pop) = 
    PoseidonSample
        { posSamIndividualID      = id
        , posSamCollectionID      = Nothing
        , posSamSourceTissue      = Nothing
        , posSamCountry           = Nothing
        , posSamLocation          = Nothing
        , posSamSite              = Nothing
        , posSamLatitude          = Nothing
        , posSamLongitude         = Nothing
        , posSamDateC14Labnr      = Nothing
        , posSamDateC14UncalBP    = Nothing
        , posSamDateC14UncalBPErr = Nothing
        , posSamDateBCADMedian    = Nothing
        , posSamDateBCADStart     = Nothing
        , posSamDateBCADStop      = Nothing
        , posSamDateType          = Nothing
        , posSamNrLibraries       = Nothing
        , posSamDataType          = Nothing
        , posSamGenotypePloidy    = Nothing
        , posSamGroupName         = [pop]
        , posSamGeneticSex        = sex
        , posSamNrAutosomalSNPs   = Nothing
        , posSamCoverage1240K     = Nothing
        , posSamMTHaplogroup      = Nothing
        , posSamYHaplogroup       = Nothing
        , posSamEndogenous        = Nothing
        , posSamUDG               = Nothing
        , posSamLibraryBuilt      = Nothing
        , posSamDamage            = Nothing
        , posSamNuclearContam     = Nothing
        , posSamNuclearContamErr  = Nothing
        , posSamMTContam          = Nothing
        , posSamMTContamErr       = Nothing
        , posSamPrimaryContact    = Nothing
        , posSamPublication       = Nothing
        , posSamComments          = Nothing
        , posSamKeywords          = Nothing
    }

-- Janno consistency checks

checkJannoConsistency :: FilePath -> [PoseidonSample] -> Either PoseidonException [PoseidonSample]
checkJannoConsistency jannoPath xs
    | not $ checkIndividualUnique xs = Left $ PoseidonJannoConsistencyException jannoPath 
        "The Individual_IDs are not unique"
    | otherwise = Right xs

checkIndividualUnique :: [PoseidonSample] -> Bool
checkIndividualUnique x = length x == length (nub $ map posSamIndividualID x)

checkJannoRowConsistency :: FilePath -> Int -> PoseidonSample -> Either PoseidonException PoseidonSample
checkJannoRowConsistency jannoPath row x
    | not $ checkMandatoryStringNotEmpty x = Left $ PoseidonJannoRowException jannoPath row
          "The mandatory columns Individual_ID and Group_Name contain empty values"
    | not $ checkC14ColsConsistent x = Left $ PoseidonJannoRowException jannoPath row
          "The columns Date_C14_Labnr, Date_C14_Uncal_BP, Date_C14_Uncal_BP_Err and Date_Type are not consistent"
    | otherwise = Right x

checkMandatoryStringNotEmpty :: PoseidonSample -> Bool
checkMandatoryStringNotEmpty x =
    not (null $ posSamIndividualID x)
    && not (null (posSamGroupName x))
    && not (null $ head $ posSamGroupName x)

checkC14ColsConsistent :: PoseidonSample -> Bool
checkC14ColsConsistent x =
    let lLabnr = maybe 0 length $ posSamDateC14Labnr x
        lUncalBP = maybe 0 length $ posSamDateC14UncalBP x
        lUncalBPErr = maybe 0 length $ posSamDateC14UncalBPErr x
        shouldBeTypeC14 = lUncalBP > 0
        isTypeC14 = posSamDateType x == Just C14
    in
        (lLabnr == 0 || lUncalBP == 0 || lLabnr == lUncalBP)
        && (lLabnr == 0 || lUncalBPErr == 0 || lLabnr == lUncalBPErr)
        && lUncalBP == lUncalBPErr
        && ((not shouldBeTypeC14) || isTypeC14)
