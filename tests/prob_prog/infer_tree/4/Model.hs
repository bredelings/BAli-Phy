module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability
import           Bio.Alphabet
import           Bio.Alignment
import           Bio.Sequence
import           Tree
import           Tree.Newick
import           SModel

model seqData = do

    let taxa = zip [0..] $ getTaxa seqData

    age    <- sample $ gamma 0.5 2
    tree   <- addLabels taxa <$> sample (uniformTimeTree age (length taxa))

    freqs  <- sample $ symmetricDirichletOn (letterSet dna) 1
    kappa1 <- sample $ logNormal 0 1
    kappa2 <- sample $ logNormal 0 1

    let tn93_model = tn93' dna kappa1 kappa2 freqs

    observe seqData $ phyloCTMC tree (alignmentLength seqData) tn93_model 1

    return ["tree" %=% writeNewick tree,
            "age" %=% age,
            "tn93:kappa1" %=% kappa1,
            "tn93:kappa2" %=% kappa2,
            "tn93:frequencies" %=% freqs]

main = do
    (options, filename) <- execParser $
      modelRunParserWith "Model" 200000 $
        strArgument (metavar "ALIGNMENT" <> help "Aligned DNA sequences")

    runInfo <- initializeModelRun (runMode options)

    seqData <- mkAlignedCharacterData dna <$> loadSequences filename

    mcmcState <- makeLoggedMCMCState runInfo (logFormats options) $ model seqData

    case runInfo of
      TestRun -> printInitialModel (logFormats options) mcmcState
      MCMCRun directory -> do
        reportModelRun (iterations options) (logFormats options) directory
        runMCMC (iterations options) mcmcState
