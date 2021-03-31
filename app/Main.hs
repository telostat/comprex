module Main where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Version           (showVersion)
import qualified Options.Applicative    as OA
import           Paths_comprex          (version)
import           System.Comprex         (programAnalyze, programCleanup, withExistingDirectory)
import           System.Exit            (ExitCode(..), exitWith)
import           System.IO              (hPutStrLn, stderr)

-- | Main program entry point.
main :: IO ()
main = exitWith =<< (cliProgram =<< OA.execParser cliProgramParserInfo)


-- | CLI program.
cliProgram :: CliArguments -> IO ExitCode
cliProgram (CliArguments (CommandAnalyze d)) = handleResult =<< withExistingDirectory d programAnalyze
cliProgram (CliArguments (CommandCleanup d)) = handleResult =<< withExistingDirectory d programCleanup


-- | Handles the program execution result.
handleResult :: MonadIO m => Maybe () -> m ExitCode
handleResult Nothing  = liftIO (hPutStrLn stderr "Path does not point to an existing directory") >> pure (ExitFailure 1)
handleResult (Just _) = pure ExitSuccess


-- | Registry of commands and their arguments.
data Command =
    CommandAnalyze FilePath
  | CommandCleanup FilePath
  deriving Show


-- | CLI arguments parser.
parserProgramOptions :: OA.Parser CliArguments
parserProgramOptions = CliArguments <$> OA.hsubparser
  (  OA.command "analyze" (OA.info (CommandAnalyze <$> optsAnalyze) (OA.progDesc "Analyze a given directory and report"))
  <> OA.command "cleanup" (OA.info (CommandCleanup <$> optsCleanup) (OA.progDesc "Remove uncompressed files that are successfully compressed"))
  )


-- | @analyze@ command arguments parser.
optsAnalyze :: OA.Parser FilePath
optsAnalyze = OA.strOption (OA.long "dir" <> OA.short 'd' <> OA.metavar "DIR" <> OA.help "Directory to analyze")


-- | @cleanup@ command arguments parser.
optsCleanup :: OA.Parser FilePath
optsCleanup = OA.strOption (OA.long "dir" <> OA.short 'd' <> OA.metavar "DIR" <> OA.help "Directory to cleanup")


-- | Parsed command line arguments.
newtype CliArguments = CliArguments { cliArgumentsCommand :: Command } deriving Show


-- | CLI program information.
cliProgramParserInfo :: OA.ParserInfo CliArguments
cliProgramParserInfo = OA.info
  (OA.helper <*> parserVersionOption <*> parserProgramOptions)
  (OA.fullDesc <> OA.progDesc "comprex" <> OA.header "comprex - Analyze and Cleanup Directory of Compressed Files")


-- | Version option.
parserVersionOption :: OA.Parser (a -> a)
parserVersionOption = OA.infoOption (showVersion version) (OA.long "version" <> OA.help "Show version")
