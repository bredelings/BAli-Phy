{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BAliPhy.Run
    ( LogFormat(..)
    , ModelRunOptions(..)
    , ModelRun(..)
    , modelRunOptions
    , prepareModelRun
    , makeModelContext
    , reportModelRun
    , printInitialModel
    , getVerbosity
    , prepareOutputFiles
    ) where

import BAliPhy.Util
import Compiler.Base
import Compiler.Classes
import Control.Applicative
import Control.Monad
import Data.Bool
import Data.Either
import Data.Eq
import Data.Function
import Data.Functor
import Data.List
import Data.JSON (Object)
import Data.Maybe
import Data.Semigroup ((<>))
import Foreign.Maybe ()
import Foreign.String
import Foreign.Vector
import MCMC
import Options.Applicative
import Probability
import Probability.Logger
import System.Exit
import System.FilePath
import System.IO
import Text.Show
import qualified Data.Text.IO as T

data LogFormat = JSON | TSV
    deriving (Eq)

data ModelRunOptions a = ModelRunOptions
    { iterations :: Int
    , testMode :: Bool
    , logFormats :: [LogFormat]
    , outputName :: String
    , modelInputs :: a
    }

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

-- Compose the controls common to standalone model programs with a typed model-input parser.
modelRunOptions :: String -> Int -> Parser a -> Parser (ModelRunOptions a)
modelRunOptions defaultName defaultIterations inputs =
    ModelRunOptions
        <$> option auto
            (short 'i' <> long "iterations" <> value defaultIterations <> showDefault <>
             metavar "N" <> help "Number of MCMC iterations")
        <*> switch (short 't' <> long "test" <> help "Analyze the initial values and exit")
        <*> option (eitherReader readLogFormats)
            (short 'l' <> long "log-format" <> value [JSON] <> showDefaultWith showLogFormats <>
             metavar "FORMAT" <> help "Scalar log format: json, tsv, or json,tsv")
        <*> strOption
            (short 'n' <> long "name" <> value defaultName <> showDefaultWith id <>
             metavar "NAME" <> help "Name for a unique output directory")
        <*> inputs

-- Represent test mode without filesystem output, or create the unique directory for an MCMC run.
prepareModelRun :: Bool -> String -> IO ModelRun
prepareModelRun True _ = return TestRun
prepareModelRun False name = do
    directory <- createUniqueDirectory name
    hPutStrLn stderr $ "Created directory " ++ show directory ++ " for output files.\n"
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

-- Create the MCMC context and attach standard parameter and density loggers for a normal run.
makeModelContext :: ModelRun -> [LogFormat] -> Random Object -> IO ContextIndex
makeModelContext TestRun _ model = makeMCMCModel model
makeModelContext (MCMCRun directory) formats model = do
    jsonLog <- if JSON `elem` formats
        then Just <$> jsonLogger (directory </> "C1.log.json")
        else return Nothing
    tsvLog <- if TSV `elem` formats
        then Just <$> tsvLogger (directory </> "C1.log") ["iter"]
        else return Nothing
    makeMCMCModel $ makeLoggedModel model jsonLog tsvLog

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

-- Print the iteration-zero representation in the same TSV-then-JSON order as the old wrapper.
printInitialModel :: [LogFormat] -> ContextIndex -> IO ()
printInitialModel formats context = do
    when (TSV `elem` formats) $ logTableLine context 0 >>= T.putStrLn
    when (JSON `elem` formats) $ logJSONLine context 0 >>= T.putStrLn

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
