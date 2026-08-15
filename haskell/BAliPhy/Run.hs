{-# LANGUAGE OverloadedStrings #-}
module BAliPhy.Run
    ( LogFormat(..)
    , ModelRunOptions(..)
    , ModelRunMode(..)
    , ModelRun(..)
    , modelRunOptions
    , modelRunParser
    , modelRunParserWith
    , withModelDescription
    , initializeModelRun
    , makeLoggedMCMCState
    , reportModelRun
    , printInitialModel
    , getVerbosity
    , prepareOutputFiles
    ) where

import BAliPhy.Util
import Data.JSON (Object)
import MCMC
import Options.Applicative
import Probability
import Probability.Logger
import System.Directory (doesDirectoryExist)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import qualified Data.Text.IO as T

data LogFormat = JSON | TSV
    deriving (Eq)

data ModelRunOptions = ModelRunOptions
    { iterations :: Int
    , logFormats :: [LogFormat]
    , runMode :: ModelRunMode
    }

data ModelRunMode
    = TestMode
    | CreateMCMCDirectory String
    | UseMCMCDirectory FilePath
    deriving (Eq, Show)

data ModelRun
    = TestRun
    | MCMCRun FilePath

-- Accept exactly the scalar formats supported by the former generated custom-model wrapper.
readLogFormats :: String -> Either String [LogFormat]
readLogFormats "json" = Right [JSON]
readLogFormats "tsv" = Right [TSV]
readLogFormats "json,tsv" = Right [JSON, TSV]
readLogFormats "tsv,json" = Right [TSV, JSON]
readLogFormats value = Left $ "unknown log format `" ++ value ++ "' (expected json, tsv, or both)"

showLogFormats :: [LogFormat] -> String
showLogFormats = intercalate "," . map showLogFormat where
    showLogFormat JSON = "json"
    showLogFormat TSV = "tsv"

-- Resolve test mode first, then an exact directory, and otherwise create a uniquely named directory.
resolveRunMode :: Bool -> Maybe FilePath -> String -> ModelRunMode
resolveRunMode True _ _ = TestMode
resolveRunMode False (Just directory) _ = UseMCMCDirectory directory
resolveRunMode False Nothing name = CreateMCMCDirectory name

-- Parse the controls common to standalone model programs with caller-selected scalar log defaults.
modelRunOptions :: String -> Int -> [LogFormat] -> Parser ModelRunOptions
modelRunOptions defaultName defaultIterations defaultLogFormats =
    ModelRunOptions
        <$> option auto
            (short 'i' <> long "iterations" <> value defaultIterations <> showDefault <>
             metavar "N" <> help "Number of MCMC iterations")
        <*> option (eitherReader readLogFormats)
            (short 'l' <> long "log-format" <> value defaultLogFormats <> showDefaultWith showLogFormats <>
             metavar "FORMAT" <> help "Scalar log format: json, tsv, or json,tsv")
        <*> (resolveRunMode
            <$> switch (short 't' <> long "test" <> help "Analyze the initial values and exit")
            <*> optional (strOption
                (long "output-dir" <> metavar "DIRECTORY" <>
                 help "Use this existing directory instead of creating one from --name"))
            <*> strOption
                (short 'n' <> long "name" <> value defaultName <> showDefaultWith id <>
                 metavar "NAME" <> help "Name for a unique output directory"))

-- Supply the standard help behavior for a model with no model-specific inputs.
modelRunParser :: String -> Int -> ParserInfo ModelRunOptions
modelRunParser defaultName defaultIterations =
    info (modelRunOptions defaultName defaultIterations [JSON] <**> helper) fullDesc

-- Parse model-specific inputs separately from the options shared by all model programs.
modelRunParserWith :: String -> Int -> Parser a -> ParserInfo (ModelRunOptions, a)
modelRunParserWith defaultName defaultIterations inputs =
    info (((,) <$> modelRunOptions defaultName defaultIterations [JSON] <*> inputs) <**> helper) fullDesc

withModelDescription :: String -> ParserInfo a -> ParserInfo a
withModelDescription description parserInfo =
    parserInfo { infoProgDesc = description }

-- Resolve a run request to either no output or the exact directory used by every logger.
initializeModelRun :: ModelRunMode -> IO ModelRun
initializeModelRun TestMode = return TestRun
initializeModelRun (CreateMCMCDirectory name) = do
    directory <- createUniqueDirectory name
    hPutStrLn stderr $ "Created directory " ++ show directory ++ " for output files.\n"
    return $ MCMCRun directory
initializeModelRun (UseMCMCDirectory directory) = do
    exists <- doesDirectoryExist directory
    unless exists $ do
        hPutStrLn stderr $ "Output directory " ++ show directory ++ " does not exist or is not a directory."
        exitFailure
    return $ MCMCRun directory

-- Retain the returned model value as the logging head and attach the selected standard scalar logs.
makeLoggedModel model jsonLog tsvLog = do
    parameters <- model
    let loggerValues =
            LoggerValues
                parameters
                (contextFields
                    ["prior" %=! logPrior, "likelihood" %=! logLikelihood,
                     "posterior" %=! logPosterior])
    case tsvLog of
        Just logger -> void $ addLogger $ logger loggerValues
        Nothing -> return ()
    case jsonLog of
        Just logger -> void $ addLogger $ logger loggerValues
        Nothing -> return ()
    return parameters

-- Create the MCMC state and attach standard parameter and density loggers for a normal run.
makeLoggedMCMCState :: ModelRun -> [LogFormat] -> Random Object -> IO ContextIndex
makeLoggedMCMCState TestRun _ model = makeMCMCState model
makeLoggedMCMCState (MCMCRun directory) formats model = do
    jsonLog <- if JSON `elem` formats
        then Just <$> jsonLogger (directory </> "C1.log.json")
        else return Nothing
    tsvLog <- if TSV `elem` formats
        then Just <$> tsvLogger (directory </> "C1.log") ["iter"]
        else return Nothing
    makeMCMCState $ makeLoggedModel model jsonLog tsvLog

-- Report the scalar destinations and stopping policy after the context and its loggers are ready.
reportModelRun :: Int -> [LogFormat] -> FilePath -> IO ()
reportModelRun maxIterations formats directory = do
    putStrLn ""
    putStrLn "Beginning MCMC computations."
    when (TSV `elem` formats) $
        putStrLn $ "   - Sampled numerical parameters logged to " ++ show (directory </> "C1.log") ++ " as TSV"
    when (JSON `elem` formats) $
        putStrLn $ "   - Sampled numerical parameters logged to " ++ show (directory </> "C1.log.json") ++ " as JSON"
    putStrLn ""
    putStrLn "BAli-Phy does NOT detect how many iterations is sufficient:"
    putStrLn "   You need to monitor convergence and kill it when done."
    putStrLn $ "   Maximum number of iterations set to " ++ show maxIterations ++ "."
    putStrLn ""
    when (TSV `elem` formats) $
        putStrLn $ "You can examine 'C1.log' using BAli-Phy tool statreport (command-line) " ++
                   "or the BEAST program Tracer (graphical)."
    putStrLn "See the manual at http://www.bali-phy.org/README.xhtml for further information."
    hFlush stdout

-- Print the iteration-zero representation and, when verbose, the model's trace graph.
printInitialModel :: [LogFormat] -> ContextIndex -> IO ()
printInitialModel formats context = do
    when (TSV `elem` formats) $ logTableLine context 0 >>= T.putStrLn
    when (JSON `elem` formats) $ logJSONLine context 0 >>= T.putStrLn
    verbosity <- getVerbosity
    when (verbosity > 0) $ writeTraceGraph context

foreign import bpcall "Environment:"
    getVerbosity :: IO Int

foreign import bpcall "File:"
    reserveOutputFilesRaw :: EVector CPPString -> IO (EVector CPPString)

-- Claim every logger path before any logger can truncate it, or return all paths already present.
reserveOutputFiles :: [FilePath] -> IO [FilePath]
reserveOutputFiles filenames = do
    collisions <- reserveOutputFilesRaw (toVector [pack_cpp_string filename | filename <- filenames])
    return [unpack_cpp_string filename | filename <- vectorToList collisions]

-- Explicit overwrite leaves logger opening unchanged; otherwise the native operation claims
-- the entire destination set before this function allows model construction to continue.
prepareOutputFiles :: Bool -> [FilePath] -> IO ()
prepareOutputFiles True _ = return ()
prepareOutputFiles False filenames = do
    collisions <- reserveOutputFiles filenames
    case collisions of
        [] -> return ()
        _ -> do
            hPutStrLn stderr "Refusing to overwrite existing BAli-Phy output files:"
            mapM_ (\filename -> hPutStrLn stderr $ "  " ++ filename) collisions
            hPutStrLn stderr "Choose another directory with --output-dir, or pass --overwrite."
            exitFailure
