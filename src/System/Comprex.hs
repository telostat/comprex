-- | Comprex library and helper definitions.
--
module System.Comprex where

import qualified Codec.Compression.GZip as GZ
import           Control.Monad.Catch    (MonadThrow)
import           Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Crypto.Hash.SHA1       as SHA1
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as D
import qualified Data.ByteString.Lazy   as BL
import           Data.List              (isSuffixOf)
import           Data.Maybe             (catMaybes)
import qualified Path                   as P
import qualified Path.IO                as PIO
import           System.IO              (hPutStrLn, stderr)
import           Text.Printf            (printf)


-- | Analyze program.
programAnalyze :: (MonadThrow m, MonadIO m) => Directory -> m ()
programAnalyze d = do
  filePairs <- catMaybes <$> (mapM getCompressedFilePair =<< listFilesUncompressed d)
  hashes <- mapM computeHashes filePairs
  liftIO $ putStrLn "FileA,HashA,FileB,HashB,Match"
  mapM_ printPair hashes
  where
    printPair ((u, hu), (c, hc)) = liftIO . putStrLn $ printf "%s,%s,%s,%s,%s" (show u) (show hu) (show c) (show hc) (show $ hu == hc)


-- | Cleanup program.
programCleanup :: MonadIO m => Directory -> m ()
programCleanup _ = liftIO $ hPutStrLn stderr "Not implemented yet..."


-- | Type alias for absolute directory paths we use across the module.
type Directory = P.Path P.Abs P.Dir


-- | Type alias for absolute file paths we use across the module.
type File = P.Path P.Abs P.File


-- | Attempts to build a 'Directory' value that is an absolute path to an
-- existing directory from the given 'FilePath' value.
--
-- >>> getExistingDirectory "/tmp" :: IO (Maybe Directory)
-- Just "/tmp/"
-- >>> getExistingDirectory "/tmp/../" :: IO (Maybe Directory)
-- Just "/"
-- >>> ((\x y -> (==) <$> x <*> Just y) <$> (getExistingDirectory "./") <*> PIO.getCurrentDir) :: IO (Maybe Bool)
-- Just True
-- >>> getExistingDirectory "/tmp" :: IO (Maybe Directory)
-- Just "/tmp/"
-- >>> getExistingDirectory "/something-very-unlikely-to-exist" :: IO (Maybe Directory)
-- Nothing
getExistingDirectory :: MonadIO m => FilePath -> m (Maybe Directory)
getExistingDirectory d = do
  p <- PIO.resolveDir' d
  e <- PIO.doesDirExist p
  pure $ if e then Just p else Nothing


-- | Attempts to get the existing 'Directory' for the given 'FilePath' and
-- performs the given action on it.
--
-- Returns 'Nothing' if the given 'FilePath' does not point to an existing
-- directory, 'Just' action return value otherwise.
--
-- >>> withExistingDirectory "/tmp" PIO.doesDirExist :: IO (Maybe Bool)
-- Just True
withExistingDirectory :: MonadIO m => FilePath -> (Directory -> m a) -> m (Maybe a)
withExistingDirectory d action = do
  mp <- getExistingDirectory d
  case mp of
    Nothing -> pure Nothing
    Just p  -> Just <$> action p


-- | Computes hashes for two given files, first uncompressed and then then the
-- second (supposedly) compressed of the first.
computeHashes :: MonadIO m => (File, File) -> m ((File, B.ByteString), (File, B.ByteString))
computeHashes (u, c) = do
  hu <- computeHash False u
  hc <- computeHash True c
  pure ((u, hu), (c, hc))


-- | Computes the hash for a given file based on whether it is compressed or not.
computeHash :: MonadIO m => Bool -> File -> m B.ByteString
computeHash c p = D.encode . SHA1.hashlazy . unContents <$> liftIO (BL.readFile $ P.toFilePath p)
  where
    unContents = if c then GZ.decompress else id


-- | Lists uncompressed files.
listFilesUncompressed :: MonadIO m => Directory -> m [File]
listFilesUncompressed = fmap (filter (not . isCompressed . P.toFilePath . P.filename) . snd) . PIO.listDir


-- | Attempts to find the compressed file for the given uncompressed file.
getCompressedFilePair :: (MonadThrow m, MonadIO m) => File -> m (Maybe (File, File))
getCompressedFilePair fp = do
  cfp <- P.addExtension ".gz" fp
  e <- PIO.doesFileExist cfp
  pure $ if e then Just (fp, cfp) else Nothing


-- | Check if the file is (supposedly) compressed.
--
-- >>> isCompressed ""
-- False
-- >>> isCompressed "gz"
-- False
-- >>> isCompressed ".gz"
-- False
-- >>> isCompressed ".GZ"
-- False
-- >>> isCompressed "a.gz"
-- True
-- >>> isCompressed "a.GZ"
-- False
isCompressed :: FilePath -> Bool
isCompressed = (&&) <$> isSuffixOf ".gz" <*> not . (==) ".gz"
