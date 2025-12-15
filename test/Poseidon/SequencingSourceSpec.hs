{-# LANGUAGE OverloadedStrings #-}

module Poseidon.SequencingSourceSpec (spec) where

import           Poseidon.AccessionIDs
import           Poseidon.ColumnTypesSSF
import           Poseidon.ColumnTypesUtils
import           Poseidon.JannoSpec        (checkEnDe)
import           Poseidon.SequencingSource (SeqSourceRow (..),
                                            SeqSourceRows (..),
                                            readSeqSourceFile)
import           Poseidon.Utils            (testLog)

import           Data.HashMap.Strict       (fromList)
import           Data.Time                 (fromGregorian)
import           Test.Hspec                (Spec, anyException, describe, it,
                                            shouldBe, shouldThrow)

spec :: Spec
spec = do
    testEnAndDecoding
    testReadSeqSourceFile

-- this mirrors the tests in JannoSpec.hs with a focus on
testEnAndDecoding :: Spec
testEnAndDecoding = describe "Poseidon.SequencingSource: JSON and CSV en- and decoding" $ do
    it "should pass smoothly through all relevant en- and decoding cycles" $ do
        -- self defined instances
        checkEnDe (enumFrom minBound :: [SSFUDG])
        checkEnDe (enumFrom minBound :: [SSFLibraryBuilt])
        -- examples from https://ena-docs.readthedocs.io/en/latest/submit/general-guide/accessions.html
        checkEnDe [
              SSFAccessionIDStudy $ INSDCProject "PRJEB12345"
            , SSFAccessionIDStudy $ INSDCStudy "ERP123456"
            --, SSFAccessionIDStudy $ OtherID "AnyString"
            ]
        checkEnDe [
              SSFAccessionIDSample $ INSDCBioSample "SAMEA123456"
            , SSFAccessionIDSample $ INSDCSample "ERS123456"
            --, SSFAccessionIDSample $ OtherID "AnyString"
            ]
        checkEnDe [
              SSFAccessionIDRun $ INSDCRun "ERR123456"
            --, SSFAccessionIDStudy $ OtherID "AnyString"
            ]
        checkEnDe [SSFFirstPublicSimpleDate $ fromGregorian 2025 7 22]
        checkEnDe [SSFLastUpdatedSimpleDate $ fromGregorian 2025 7 21]
        checkEnDe [SSFFastqFTPURI "http://www.google.de"]
        checkEnDe [SSFFastqASPERAURI "http://www.google.de"]
        checkEnDe [SSFFastqBytes 999999999999]
        checkEnDe [SSFFastqMD5 "098f6bcd4621d373cade4e832627b4f6"]
        checkEnDe [SSFReadCount 999999999999]
        checkEnDe [SSFSubmittedFTPURI "http://www.google.de"]

testReadSeqSourceFile :: Spec
testReadSeqSourceFile = describe "Poseidon.SequencingSource.readSeqSourceFile" $ do
    let normalFullSeqSourcePath = "test/testDat/testSeqSourceFiles/normal_full.ssf"
    it "should read normal .ssf files correctly" $ do
        (SeqSourceRows s) <- testLog $ readSeqSourceFile [] normalFullSeqSourcePath
        length s `shouldBe` 3
        map sPoseidonID s                `shouldBe` [ Just $ ListColumn ["Ash033.SG"]
                                                    , Just $ ListColumn ["Ash002.SG"]
                                                    , Just $ ListColumn ["Ash040.SG"]
                                                    ]
        map sUDG s                       `shouldBe` [Just SSFMinus, Just SSFHalf, Just SSFPlus]
        map sLibraryBuilt s              `shouldBe` [Just SSFSS, Just SSFDS, Just SSFDS]
        map sSampleAccession s           `shouldBe` [ Just $ SSFAccessionIDSample $ INSDCBioSample "SAMEA7050454"
                                                    , Just $ SSFAccessionIDSample $ INSDCBioSample "SAMEA7050404"
                                                    , Just $ SSFAccessionIDSample $ INSDCBioSample "SAMEA7050455"
                                                    ]
        map sStudyAccession s            `shouldBe` [ Just $ SSFAccessionIDStudy $ INSDCProject "PRJEB39316"
                                                    , Just $ SSFAccessionIDStudy $ INSDCProject "PRJEB39316"
                                                    , Just $ SSFAccessionIDStudy $ INSDCProject "PRJEB39316"
                                                    ]
        map sRunAccession s              `shouldBe` [ Just $ SSFAccessionIDRun $ INSDCRun "ERR4331996"
                                                    , Just $ SSFAccessionIDRun $ INSDCRun "ERR4332592"
                                                    , Just $ SSFAccessionIDRun $ INSDCRun "ERR4332593"
                                                    ]
        map sSampleAlias s               `shouldBe` [ Just $ SSFSampleAlias "2"
                                                    , Just $ SSFSampleAlias "1"
                                                    , Just $ SSFSampleAlias "3"
                                                    ]
        map sSecondarySampleAccession s  `shouldBe` [ Just $ SSFSecondarySampleAccession "ERS4811084"
                                                    , Just $ SSFSecondarySampleAccession "ERS4811035"
                                                    , Just $ SSFSecondarySampleAccession "ERS4811085"
                                                    ]
        map sFirstPublic s               `shouldBe` [ Just $ SSFFirstPublicSimpleDate $ fromGregorian 2021 4 12
                                                    , Just $ SSFFirstPublicSimpleDate $ fromGregorian 2021 4 12
                                                    , Just $ SSFFirstPublicSimpleDate $ fromGregorian 2021 4 12
                                                    ]
        map sLastUpdated s               `shouldBe` [ Just $ SSFLastUpdatedSimpleDate $ fromGregorian 2020 7 9
                                                    , Just $ SSFLastUpdatedSimpleDate $ fromGregorian 2020 7 10
                                                    , Just $ SSFLastUpdatedSimpleDate $ fromGregorian 2020 7 10
                                                    ]
        map sInstrumentModel s           `shouldBe` [ Just $ SSFInstrumentModel "Illumina HiSeq 2500"
                                                    , Just $ SSFInstrumentModel "Illumina HiSeq 2500"
                                                    , Just $ SSFInstrumentModel "Illumina HiSeq 2500"
                                                    ]
        map sLibraryLayout s             `shouldBe` [ Just $ SSFLibraryLayout "SINGLE"
                                                    , Just $ SSFLibraryLayout "SINGLE"
                                                    , Just $ SSFLibraryLayout "SINGLE"
                                                    ]
        map sLibrarySource s             `shouldBe` [ Just $ SSFLibrarySource "GENOMIC"
                                                    , Just $ SSFLibrarySource "GENOMIC"
                                                    , Just $ SSFLibrarySource "GENOMIC"
                                                    ]
        map sInstrumentPlatform s        `shouldBe` [ Just $ SSFInstrumentPlatform "ILLUMINA"
                                                    , Just $ SSFInstrumentPlatform "ILLUMINA"
                                                    , Just $ SSFInstrumentPlatform "ILLUMINA"
                                                    ]
        map sLibraryName s               `shouldBe` [ Just $ SSFLibraryName "Ash033_all"
                                                    , Just $ SSFLibraryName "Ash002_all"
                                                    , Just $ SSFLibraryName "Ash040_all"
                                                    ]
        map sLibraryStrategy s           `shouldBe` [ Just $ SSFLibraryStrategy "WGS"
                                                    , Just $ SSFLibraryStrategy "WGS"
                                                    , Just $ SSFLibraryStrategy "WGS"]
        map sFastqFTP s                  `shouldBe` [ Just $ ListColumn [
                                                          SSFFastqFTPURI "ftp.sra.ebi.ac.uk/vol1/fastq/ERR433/006/ERR4331996/ERR4331996.fastq.gz"
                                                        ]
                                                    , Just $ ListColumn [
                                                          SSFFastqFTPURI "ftp.sra.ebi.ac.uk/vol1/fastq/ERR433/002/ERR4332592/ERR4332592.fastq.gz"
                                                        ]
                                                    , Just $ ListColumn [
                                                          SSFFastqFTPURI "ftp.sra.ebi.ac.uk/vol1/fastq/ERR433/003/ERR4332593/ERR4332593.fastq.gz"
                                                        , SSFFastqFTPURI "ftp.sra.ebi.ac.uk/vol1/fastq/testdummy.fastq.gz"
                                                        ]
                                                    ]
        map sFastqASPERA s               `shouldBe` [ Just $ ListColumn [
                                                          SSFFastqASPERAURI "fasp.sra.ebi.ac.uk:/vol1/fastq/ERR433/006/ERR4331996/ERR4331996.fastq.gz"
                                                        ]
                                                    , Just $ ListColumn [
                                                          SSFFastqASPERAURI "fasp.sra.ebi.ac.uk:/vol1/fastq/ERR433/002/ERR4332592/ERR4332592.fastq.gz"
                                                        ]
                                                    , Just $ ListColumn [
                                                          SSFFastqASPERAURI "fasp.sra.ebi.ac.uk:/vol1/fastq/ERR433/003/ERR4332593/ERR4332593.fastq.gz"
                                                        , SSFFastqASPERAURI "fasp.sra.ebi.ac.uk:/vol1/fastq/testdummy.fastq.gz"
                                                        ]
                                                    ]
        map sFastqBytes s                `shouldBe` [ Just $ ListColumn [SSFFastqBytes 649563861]
                                                    , Just $ ListColumn [SSFFastqBytes 194164761]
                                                    , Just $ ListColumn [SSFFastqBytes 276693447, SSFFastqBytes 3]
                                                    ]
        map sFastqMD5 s                  `shouldBe` [ Just $ ListColumn [SSFFastqMD5 "9bd0fceb5ab46cb894ea33765c122e83"]
                                                    , Just $ ListColumn [SSFFastqMD5 "6d8831f5bb8ba9870cb55f834e98ab4d"]
                                                    , Just $ ListColumn [
                                                          SSFFastqMD5 "539852f3d7fb574b2a1e4f1c0059f163"
                                                        , SSFFastqMD5 "539852f3d7fb574b2a1e4f1c0059f165"
                                                        ]
                                                    ]
        map sReadCount s                 `shouldBe` [ Just $ SSFReadCount 23386349
                                                    , Just $ SSFReadCount 6471092
                                                    , Just $ SSFReadCount 9442394
                                                    ]
        map sSubmittedFTP s              `shouldBe` [ Just $ ListColumn [
                                                          SSFSubmittedFTPURI "ftp.sra.ebi.ac.uk/vol1/run/ERR433/ERR4331996/Ash033_all.merged.hs37d5.fa.cons.90perc.bam"
                                                        ]
                                                    , Just $ ListColumn [
                                                          SSFSubmittedFTPURI "ftp.sra.ebi.ac.uk/vol1/run/ERR433/ERR4332592/Ash002_all.merged.hs37d5.fa.cons.90perc.bam"
                                                        ]
                                                    , Just $ ListColumn [
                                                          SSFSubmittedFTPURI "ftp.sra.ebi.ac.uk/vol1/run/ERR433/ERR4332593/Ash040_all.merged.hs37d5.fa.cons.90perc.bam"
                                                        , SSFSubmittedFTPURI "ftp.sra.ebi.ac.uk/vol1/run/testdummy.bam"
                                                        ]
                                                    ]
        map sAdditionalColumns s         `shouldBe` [CsvNamedRecord $ fromList [], CsvNamedRecord $ fromList [], CsvNamedRecord $ fromList []]

    it "should fail to read ssf files with missing mandatory columns" $ do
        testLog (readSeqSourceFile ["Bohrmaschine"] normalFullSeqSourcePath) `shouldThrow` anyException
