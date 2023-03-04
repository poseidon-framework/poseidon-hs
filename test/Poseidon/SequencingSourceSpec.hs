{-# LANGUAGE OverloadedStrings #-}

module Poseidon.SequencingSourceSpec (spec) where

import           Poseidon.Janno            (AccessionID (..),
                                            CsvNamedRecord (..), JannoList (..))
import           Poseidon.JannoSpec        (checkEnDe)
import           Poseidon.SequencingSource (SeqSourceRow (..),
                                            SeqSourceRows (..),
                                            readSeqSourceFile, SSFUDG (..), SSFLibraryBuilt (..))
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
    it "should read normal janno files correctly" $ do
        (SeqSourceRows s) <- testLog $ readSeqSourceFile normalFullSeqSourcePath
        length s `shouldBe` 3
        map sPoseidonID s                `shouldBe` [JannoList ["Ash033.SG"], JannoList ["Ash002.SG"], JannoList ["Ash040.SG"]]
        map sUDG s                       `shouldBe` [Just SSFMinus, Just SSFHalf, Just SSFPlus]
        map sLibraryBuilt s              `shouldBe` [Just SSFSS, Just SSFDS, Just SSFDS]
        map sGeneticSourceAccessionIDs s `shouldBe` [INSDCBioSample "SAMEA7050454", INSDCBioSample "SAMEA7050404", INSDCBioSample "SAMEA7050455"]
        map sAdditionalColumns s         `shouldBe` [
              CsvNamedRecord (fromList [
                ("study_accession","PRJEB39316"),
                ("run_accession","ERR4331996"),
                ("sample_alias","2"),
                ("secondary_sample_accession","ERS4811084"),
                ("first_public","2021-04-12"),
                ("last_updated","2020-07-09"),
                ("instrument_model","Illumina HiSeq 2500"),
                ("library_layout","SINGLE"),
                ("library_source","GENOMIC"),
                ("instrument_platform","ILLUMINA"),
                ("library_name","Ash033_all"),
                ("library_strategy","WGS"),
                ("fastq_aspera","fasp.sra.ebi.ac.uk:/vol1/fastq/ERR433/006/ERR4331996/ERR4331996.fastq.gz"),
                ("fastq_bytes","649563861"),
                ("fastq_md5","9bd0fceb5ab46cb894ea33765c122e83"),
                ("fastq_ftp","ftp.sra.ebi.ac.uk/vol1/fastq/ERR433/006/ERR4331996/ERR4331996.fastq.gz"),
                ("read_count","23386349"),
                ("submitted_ftp","ftp.sra.ebi.ac.uk/vol1/run/ERR433/ERR4331996/Ash033_all.merged.hs37d5.fa.cons.90perc.bam")
                ])
            , CsvNamedRecord (fromList [
                ("study_accession","PRJEB39316"),
                ("run_accession","ERR4332592"),
                ("sample_alias","1"),
                ("secondary_sample_accession","ERS4811035"),
                ("first_public","2021-04-12"),
                ("last_updated","2020-07-10"),
                ("instrument_model","Illumina HiSeq 2500"),
                ("library_layout","SINGLE"),
                ("library_source","GENOMIC"),
                ("instrument_platform","ILLUMINA"),
                ("library_name","Ash002_all"),
                ("library_strategy","WGS"),
                ("fastq_aspera","fasp.sra.ebi.ac.uk:/vol1/fastq/ERR433/002/ERR4332592/ERR4332592.fastq.gz"),
                ("fastq_bytes","194164761"),
                ("fastq_md5","6d8831f5bb8ba9870cb55f834e98ab4d"),
                ("fastq_ftp","ftp.sra.ebi.ac.uk/vol1/fastq/ERR433/002/ERR4332592/ERR4332592.fastq.gz"),
                ("read_count","6471092"),
                ("submitted_ftp","ftp.sra.ebi.ac.uk/vol1/run/ERR433/ERR4332592/Ash002_all.merged.hs37d5.fa.cons.90perc.bam")
                ])
            , CsvNamedRecord (fromList [
                ("study_accession","PRJEB39316"),
                ("run_accession","ERR4332593"),
                ("sample_alias","3"),
                ("secondary_sample_accession","ERS4811085"),
                ("first_public","2021-04-12"),
                ("last_updated","2020-07-10"),
                ("instrument_model","Illumina HiSeq 2500"),
                ("library_layout","SINGLE"),
                ("library_source","GENOMIC"),
                ("instrument_platform","ILLUMINA"),
                ("library_name","Ash040_all"),
                ("library_strategy","WGS"),
                ("fastq_aspera","fasp.sra.ebi.ac.uk:/vol1/fastq/ERR433/003/ERR4332593/ERR4332593.fastq.gz"),
                ("fastq_bytes","276693447"),
                ("fastq_md5","539852f3d7fb574b2a1e4f1c0059f163"),
                ("fastq_ftp","ftp.sra.ebi.ac.uk/vol1/fastq/ERR433/003/ERR4332593/ERR4332593.fastq.gz"),
                ("read_count","9442394"),
                ("submitted_ftp","ftp.sra.ebi.ac.uk/vol1/run/ERR433/ERR4332593/Ash040_all.merged.hs37d5.fa.cons.90perc.bam")
                ])
            ]
