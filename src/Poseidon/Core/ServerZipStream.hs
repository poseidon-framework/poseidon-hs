{-# LANGUAGE RecordWildCards #-}

module Poseidon.Core.ServerZipStream (collectPackageZipFiles, sendPackageArchive) where

import           Poseidon.Core.GenotypeData    (GenotypeDataSpec (..),
                                                GenotypeFileSpec (..))
import           Poseidon.Core.Package         (PoseidonPackage (..))

import           Codec.Archive.Zip.Conduit.Zip (ZipData, ZipEntry (..),
                                                ZipOptions (..),
                                                defaultZipOptions, zipFileData,
                                                zipStream)
import           Control.Monad                 (void)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Resource  (MonadResource)
import           Data.ByteString.Builder       (byteString)
import qualified Data.Conduit                  as C
import qualified Data.Conduit.Combinators      as CC
import           Data.Maybe                    (maybeToList)
import qualified Data.Text                     as T
import           Data.Time.LocalTime           (LocalTime, utc, utcToLocalTime)
import           Data.Word                     (Word64)
import           Network.Wai                   (StreamingBody)
import           System.Directory              (getFileSize,
                                                getModificationTime)
import           System.FilePath               ((</>))

data PackageZipFile = PackageZipFile {
    pzfZipName  :: !T.Text
  , pzfDiskPath :: !FilePath
  , pzfModTime  :: !LocalTime
  , pzfSize     :: !Word64
  }

collectPackageZipFiles :: PoseidonPackage -> IO [PackageZipFile]
collectPackageZipFiles pac = traverse makePackageZipFile (packageFileNames pac)
  where
    makePackageZipFile :: FilePath -> IO PackageZipFile
    makePackageZipFile path = do
      let fullPath = posPacBaseDir pac </> path
      modTimeUtc <- getModificationTime fullPath
      sizeInteger <- getFileSize fullPath
      return PackageZipFile {
          pzfZipName = T.pack path
        , pzfDiskPath = fullPath
        , pzfModTime = utcToLocalTime utc modTimeUtc
        , pzfSize = fromInteger sizeInteger
        }

packageFileNames :: PoseidonPackage -> [FilePath]
packageFileNames pac =
       [ "POSEIDON.yml" ]
    ++ maybeToList (posPacJannoFile pac)
    ++ maybeToList (posPacSeqSourceFile pac)
    ++ maybeToList (posPacBibFile pac)
    ++ maybeToList (posPacReadmeFile pac)
    ++ maybeToList (posPacChangelogFile pac)
    ++ genotypeFiles
  where
    genotypeFiles = case genotypeFileSpec . posPacGenotypeData $ pac of
      GenotypeEigenstrat gf _ sf _ i _ -> [gf, sf, i]
      GenotypePlink      gf _ sf _ i _ -> [gf, sf, i]
      GenotypeVCF        gf _          -> [gf]

sendPackageArchive :: [PackageZipFile] -> StreamingBody
sendPackageArchive zipFiles send flush = do
  C.runConduitRes $
         sourcePackageZipFiles zipFiles
    C..| void (zipStream zipOptions)
    C..| CC.mapM_ (liftIO . send . byteString)
  flush

zipOptions :: ZipOptions
zipOptions = defaultZipOptions { zipOptCompressLevel = 0 }

sourcePackageZipFiles :: MonadResource m => [PackageZipFile] -> C.ConduitM () (ZipEntry, ZipData m) m ()
sourcePackageZipFiles = mapM_ $ \PackageZipFile{..} -> do
  let entry = ZipEntry {
      zipEntryName = Left pzfZipName
    , zipEntryTime = pzfModTime
    , zipEntrySize = Just pzfSize
    , zipEntryExternalAttributes = Nothing
    }
  C.yield (entry, zipFileData pzfDiskPath)

