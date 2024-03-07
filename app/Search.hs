module Search where

import Control.Exception (catch, SomeException)
import Data.List (isInfixOf)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word64)
import GHC.Exts (fromList)
import Options.Applicative ( (<**>)
                           , auto
                           , execParserPure
                           , handleParseResult
                           , help
                           , helper
                           , info
                           , long
                           , metavar
                           , option
                           , Parser
                           , prefs
                           , progDesc
                           , short
                           , some
                           , strOption
                           )
import System.Environment (getArgs)
import System.Random (newStdGen)

import Choreography

type Logger = String -> IO ()

data Arguments = Arguments { sizing :: ProgramSize
                           , destination :: FilePath
                           , filePrefix :: String
                           , iters :: IterConfig
                           , alpha :: PValue
                           }

argParser :: Parser Arguments
argParser = do
    len <- option auto (            long "body-length"       <> short 'l' <> metanat
                                    <> help "Number of lines in the heart of the program.")
    algWidth <- option auto (       long "alg-width"         <> short 'a' <> metanat
                                    <> help "Maximum terms in a computation line.")
    secWidth <- option auto (       long "secrets"           <> short 's' <> metanat
                                    <> help "How many secrets each party has.")
    flipWidth <- option auto (      long "flips"             <> short 'f' <> metanat
                                    <> help "How many flips each party has.")
    outWidth <- option auto (       long "outputs"           <> short 'o' <> metanat
                                    <> help "Max outputs each party may have.")
    participants <- fromList <$> some (Party <$> strOption (
                                    long "party"             <> short 'p' <> metaname
                                                                    <> help "Name of a participating party."))
    compute <- option auto (        long "compute-frequency" <> long "cf" <> metanat
                                    <> help "Relative frequency of compute lines, compared to sends and OTs.")
    send <- option auto (           long "send-frequency"    <> long "sf" <> metanat
                                    <> help "Relative frequency of send lines, compared to compute and OTs.")
    obliv <- option auto (          long "obliv-frequency"   <> long "of" <> metanat
                                    <> help "Relative frequency of OT lines, compared to compute and sends.")
    xorPreference <- option auto (  long "xor-to-and-ratio"  <> short 'x' <> metanat
                                    <> help "How many times more common XOR is than AND.")
    notFrequency <- option auto (   long "not-frequency"     <> short 'n' <> metaprob
                                    <> help "How likely each sub-term of a computation is to be negated.")
    sendEagerness <- option auto (  long "send-eagerness"    <> short 'e' <> metaprob
                                    <> help ("How likely each party is to recieve each send or OT. "
                                             ++ "Because everything has to go to _someone_, the real probability will be higher."))
    oblivComplexity <- option auto (long "obliv-width"       <> short 'b' <> metaprob
                                    <> help ("The likelyhood (when there are enough distinct variables) "
                                             ++ "that each branch of an OT will be composed of further branching."))
    destination <- strOption (      long "destination"       <> short 'd' <> metavar "PATH"
                                    <> help "Directory in which to write passing protocols.")
    filePrefix <- strOption (       long "prefix"            <> long "px" <> metavar "WORD"
                                    <> help "Prefix for sequential filenames, to distinguish between runs.")
    iterations <- option auto (     long "iterations"        <> short 'i' <> metavar "ITERATIONS"
                                    <> help "How many times the cycle to testing and training a decision tree will repeat.")
    trainingN <- option auto (      long "trainingN"         <> long "tR" <> metanat
                                    <> help "How many rows of data to train the decision trees on. Should be a multiple of 64.")
    testingN <- option auto (       long "testingN"          <> long "tS" <> metanat
                                    <> help "How many rows of data to test the decision trees on. Should be a multiple of 64.")
    alpha <- option auto (          long "alpha"          <> short 't' <> metaprob
                                    <> help "The significance threshold for p-value testing. Only protocols that _fail_ this threshold will be saved.")
    return Arguments{destination, filePrefix,
                     sizing = ProgramSize{len, algWidth, secWidth, flipWidth, outWidth, participants,
                                          bodyRatios = BodyRatios{compute, send, obliv},
                                          xorPreference, notFrequency, sendEagerness, oblivComplexity},
                     iters = IterConfig{iterations, trainingN, testingN},
                     alpha}
  where metanat = metavar "NATURAL"
        metaname = metavar "NAME"
        metaprob = metavar "PROBABILITY"

main :: IO ()
main = do args <- getArgs
          Arguments{destination, filePrefix, sizing, iters, alpha} <- handleParseResult $
            execParserPure (prefs mempty)
                           (info (argParser <**> helper)
                                 (progDesc $ "Search for randomly-generated .cho protocols that can't be detected insecure "
                                           ++ " using the provided dtree settings.")) args
          time <- getCurrentTime
          let runName = filePrefix ++ formatTime defaultTimeLocale "%04Y_%b_%d_%H_%M_%S_" time
          let fileNames = [destination ++ "/" ++ runName ++ show i ++ ".cho"
                           | i :: Integer <- [1..]]
          let writeLog = appendFile $ destination ++ "/" ++ runName ++ "log.txt"
          mapM_ (blindDetermination writeLog . attempt writeLog sizing iters alpha) fileNames

attempt :: Logger -> ProgramSize -> IterConfig -> PValue -> FilePath -> IO ()
attempt writeLog sizing iters alpha destination = do
  q <- newStdGen
  let cho = either error id . validate mempty . snd . fakePos 0 $ randomProgram sizing q
  writeLog $ "Generated " ++ show (length cho) ++ " line program. "
  pval <- experiment_ @Word64 iters cho corruption
  writeLog $ "Measured p-value: " ++ show pval ++ " "
  if pval `indicatesSecurityBy` alpha
    then do writeFile destination $ unlines [makeHeader sizing iters pval,
                                             render cho]
            writeLog $ "Wrote out to \"" ++ destination ++ "\"."
    else writeLog "Discarding. "

blindDetermination :: Logger -> IO () -> IO ()
blindDetermination writeLog task = do catch task \(e :: SomeException) -> let m = show e
                                                                          in writeLog if "ValueError: Found array with 0 feature(s)" `isInfixOf` m
                                                                                        then "Ignoring un-assessable protocol."
                                                                                        else m
                                      writeLog "\n"

corruption :: PartySet
corruption = Parties $ fromList [corrupt, p1]

makeHeader :: ProgramSize -> IterConfig -> PValue -> String
makeHeader sizing iters pval = unlines [ "{-"
                                       , show sizing
                                       , show iters
                                       , show pval
                                       , "-}"
                                       ]

