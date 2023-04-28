{-# LANGUAGE OverloadedStrings #-}

module Poseidon.SequencingSourceSpec (spec) where

import           Poseidon.Janno            (AccessionID (..),
                                            CsvNamedRecord (..), JURI (..),
                                            JannoList (..))
import           Poseidon.JannoSpec        (checkEnDe)
import           Poseidon.SequencingSource (SSFLibraryBuilt (..), SSFUDG (..),
                                            SeqSourceRow (..),
                                            SeqSourceRows (..),
                                            readSeqSourceFile)
import           Poseidon.Utils            (testLog)

import           Data.HashMap.Strict       (fromList)
import           Test.Hspec                (Spec, describe, it, shouldBe)

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
        checkEnDe [ -- examples from https://ena-docs.readthedocs.io/en/latest/submit/general-guide/accessions.html
              INSDCProject "PRJEB12345"
            , INSDCStudy "ERP123456"
            , INSDCBioSample "SAMEA123456"
            , INSDCSample "ERS123456"
            , INSDCExperiment "ERX123456"
            , INSDCRun "ERR123456"
            , INSDCAnalysis "ERZ123456"
            , OtherID "AnyString"
            ]

testReadSeqSourceFile :: Spec
testReadSeqSourceFile = describe "Poseidon.SequencingSource.readSeqSourceFile" $ do
    let normalFullSeqSourcePath = "test/testDat/testSeqSourceFiles/normal_full.ssf"
    it "should read normal .ssf files correctly" $ do
        (SeqSourceRows s) <- testLog $ readSeqSourceFile normalFullSeqSourcePath
        length s `shouldBe` 3
        map sPoseidonID s                `shouldBe` [JannoList ["Ash033.SG"], JannoList ["Ash002.SG"], JannoList ["Ash040.SG"]]
        map sUDG s                       `shouldBe` [Just SSFMinus, Just SSFHalf, Just SSFPlus]
        map sLibraryBuilt s              `shouldBe` [Just SSFSS, Just SSFDS, Just SSFDS]
        map sGeneticSourceAccessionIDs s `shouldBe` [INSDCBioSample "SAMEA7050454", INSDCBioSample "SAMEA7050404", INSDCBioSample "SAMEA7050455"]
        map sStudyAccession s            `shouldBe` [Just $ INSDCProject "PRJEB39316", Just $ INSDCProject "PRJEB39316", Just $ INSDCProject "PRJEB39316"]
        map sRunAccession s              `shouldBe` [Just $ INSDCRun "ERR4331996", Just $ INSDCRun "ERR4332592", Just $ INSDCRun "ERR4332593"]
        map sSampleAlias s               `shouldBe` [Just "2", Just "1", Just "3"]
        map sSecondarySampleAccession s  `shouldBe` [Just "ERS4811084", Just "ERS4811035", Just "ERS4811085"]
        map sFirstPublic s               `shouldBe` [Just "2021-04-12", Just "2021-04-12", Just "2021-04-12"]
        map sLastUpdated s               `shouldBe` [Just "2020-07-09", Just "2020-07-10", Just "2020-07-10"]
        map sInstrumentModel s           `shouldBe` [Just "Illumina HiSeq 2500", Just "Illumina HiSeq 2500", Just "Illumina HiSeq 2500"]
        map sLibraryLayout s             `shouldBe` [Just "SINGLE", Just "SINGLE", Just "SINGLE"]
        map sLibrarySource s             `shouldBe` [Just "GENOMIC", Just "GENOMIC", Just "GENOMIC"]
        map sInstrumentPlatform s        `shouldBe` [Just "ILLUMINA", Just "ILLUMINA", Just "ILLUMINA"]
        map sLibraryName s               `shouldBe` [Just "Ash033_all", Just "Ash002_all", Just "Ash040_all"]
        map sLibraryStrategy s           `shouldBe` [Just "WGS", Just "WGS", Just "WGS"]
        map sFastqFTP s                  `shouldBe` [ Just $ JannoList [JURI "ftp.sra.ebi.ac.uk/vol1/fastq/ERR433/006/ERR4331996/ERR4331996.fastq.gz"]
                                                    , Just $ JannoList [JURI "ftp.sra.ebi.ac.uk/vol1/fastq/ERR433/002/ERR4332592/ERR4332592.fastq.gz"]
                                                    , Just $ JannoList [
                                                          JURI "ftp.sra.ebi.ac.uk/vol1/fastq/ERR433/003/ERR4332593/ERR4332593.fastq.gz"
                                                        , JURI "ftp.sra.ebi.ac.uk/vol1/fastq/testdummy.fastq.gz"
                                                        ]
                                                    ]
        map sFastqASPERA s               `shouldBe` [ Just $ JannoList [JURI "fasp.sra.ebi.ac.uk:/vol1/fastq/ERR433/006/ERR4331996/ERR4331996.fastq.gz"]
                                                    , Just $ JannoList [JURI "fasp.sra.ebi.ac.uk:/vol1/fastq/ERR433/002/ERR4332592/ERR4332592.fastq.gz"]
                                                    , Just $ JannoList [
                                                          JURI "fasp.sra.ebi.ac.uk:/vol1/fastq/ERR433/003/ERR4332593/ERR4332593.fastq.gz"
                                                        , JURI "fasp.sra.ebi.ac.uk:/vol1/fastq/testdummy.fastq.gz"
                                                        ]
                                                    ]
        map sFastqBytes s                `shouldBe` [Just $ JannoList [649563861], Just $ JannoList [194164761], Just $ JannoList [276693447, 3]]
        map sFastqMD5 s                  `shouldBe` [ Just $ JannoList ["9bd0fceb5ab46cb894ea33765c122e83"]
                                                    , Just $ JannoList ["6d8831f5bb8ba9870cb55f834e98ab4d"]
                                                    , Just $ JannoList ["539852f3d7fb574b2a1e4f1c0059f163", "539852f3d7fb574b2a1e4f1c0059f165"]
                                                    ]
        map sReadCount s                 `shouldBe` [Just 23386349, Just 6471092, Just 9442394]
        map sSubmittedFTP s              `shouldBe` [ Just $ JannoList [JURI "ftp.sra.ebi.ac.uk/vol1/run/ERR433/ERR4331996/Ash033_all.merged.hs37d5.fa.cons.90perc.bam"]
                                                    , Just $ JannoList [JURI "ftp.sra.ebi.ac.uk/vol1/run/ERR433/ERR4332592/Ash002_all.merged.hs37d5.fa.cons.90perc.bam"]
                                                    , Just $ JannoList [
                                                          JURI "ftp.sra.ebi.ac.uk/vol1/run/ERR433/ERR4332593/Ash040_all.merged.hs37d5.fa.cons.90perc.bam"
                                                        , JURI "ftp.sra.ebi.ac.uk/vol1/run/testdummy.bam"
                                                        ]
                                                    ]
        map sAdditionalColumns s         `shouldBe` [CsvNamedRecord $ fromList [], CsvNamedRecord $ fromList [], CsvNamedRecord $ fromList []]
