{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poseidon.Core.ServerZipStream (collectPackageZipFiles, sendPackageArchive, unzipPackage) where

import           Poseidon.Core.GenotypeData      (GenotypeDataSpec (..),
                                                  GenotypeFileSpec (..))
import           Poseidon.Core.Package           (PoseidonPackage (..))
import           Poseidon.Core.Utils

import           Codec.Archive.Zip.Conduit.UnZip (unZipStream)
import           Codec.Archive.Zip.Conduit.Zip   (ZipData, ZipEntry (..),
                                                  ZipOptions (..),
                                                  defaultZipOptions,
                                                  zipFileData, zipStream)
import           Control.Exception               (SomeException, catch, throwIO)
import           Control.Monad                   (unless, void)
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.Resource    (MonadResource)
import qualified Data.ByteString                 as BS
import           Data.ByteString.Builder         (byteString)
import qualified Data.ByteString.Char8           as B8
import qualified Data.Conduit                    as C
import qualified Data.Conduit.Combinators        as CC
import           Data.Maybe                      (maybeToList)
import qualified Data.Text                       as T
import           Data.Time.LocalTime             (LocalTime, utc,
                                                  utcToLocalTime)
import           Data.Word                       (Word64)
import           GHC.IORef
import           Network.Wai                     (StreamingBody)
import           System.Directory                (createDirectoryIfMissing,
                                                  getFileSize,
                                                  getModificationTime)
import           System.FilePath                 (takeDirectory, (</>))
import           System.IO                       (Handle, IOMode (..), hClose,
                                                  openBinaryFile)

-- zipping

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
zipOptions = defaultZipOptions { zipOptCompressLevel = 1 }
-- zipOptCompressLevel can not be 0, because unZipStream can not handle uncompressed archives

sourcePackageZipFiles :: MonadResource m => [PackageZipFile] -> C.ConduitM () (ZipEntry, ZipData m) m ()
sourcePackageZipFiles = mapM_ $ \PackageZipFile{..} -> do
  let entry = ZipEntry {
      zipEntryName = Left pzfZipName
    , zipEntryTime = pzfModTime
    , zipEntrySize = Just pzfSize
    , zipEntryExternalAttributes = Nothing
    }
  C.yield (entry, zipFileData pzfDiskPath)

-- unzipping

unzipPackage :: FilePath -> FilePath -> IO ()
unzipPackage zip_ outDir =
    C.runConduitRes (
           CC.sourceFile zip_
      C..| void unZipStream
      C..| sinkZipEntries outDir
      ) `catch` \(e :: SomeException) -> throwIO (PoseidonUnzipException e)

sinkZipEntries :: MonadResource m => FilePath -> C.ConduitT (Either ZipEntry BS.ByteString) C.Void m ()
sinkZipEntries outDir = C.bracketP (newIORef Nothing) closeCurrent run
  where
    run currentHandle =
      C.awaitForever $ \case
        Left entry -> liftIO $ do
          closeCurrent currentHandle
          mh <- openEntry outDir entry
          writeIORef currentHandle mh
        Right chunk -> liftIO $ do
          mh <- readIORef currentHandle
          case mh of
            Just h  -> BS.hPut h chunk
            Nothing -> unless (BS.null chunk) $ throwIO $ PoseidonUnzipException $
                          error "ZIP data chunk without current file entry"

closeCurrent :: IORef (Maybe Handle) -> IO ()
closeCurrent ref = do
  mh <- readIORef ref
  writeIORef ref Nothing
  mapM_ hClose mh

openEntry :: FilePath -> ZipEntry -> IO (Maybe Handle)
openEntry outDir entry = do
  let relPath = zipEntryNameToFilePath (zipEntryName entry)
      dest    = outDir </> relPath
  if isDirectoryPath relPath
    then do
      createDirectoryIfMissing True dest
      pure Nothing
    else do
      createDirectoryIfMissing True (takeDirectory dest)
      Just <$> openBinaryFile dest WriteMode

zipEntryNameToFilePath :: Either T.Text BS.ByteString -> FilePath
zipEntryNameToFilePath (Left t)   = T.unpack t
zipEntryNameToFilePath (Right bs) = B8.unpack bs

isDirectoryPath :: FilePath -> Bool
isDirectoryPath p = not (null p) && last p == '/'
