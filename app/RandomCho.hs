module RandomCho where

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
                           , value, strOption
                           )
import System.Environment (getArgs)
import System.Random (newStdGen)

import Choreography (BodyRatios(..), Party(Party), ProgramSize(..), randomProgram)
import Utils (pretty)

{-
data BodyRatios = BodyRatios { compute :: Natural
                             , send :: Natural
                             , obliv :: Natural
                             }

data ProgramSize = ProgramSize { len :: Int
                               , algWidth :: Int
                               , secWidth :: Int
                               , flipWidth :: Int
                               , outWidth :: Int
                               , participants :: NonEmpty Party
                               , bodyRatios :: BodyRatios
                               , xorPreference :: Natural
                               , notFrequency :: Double
                               , sendEagerness :: Double
                               , oblivComplexity :: Double
                               }
-}

data Arguments = Arguments { sizing :: ProgramSize
                           , destination :: Maybe FilePath
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
    destination <- strOption (      long "destination"       <> short 'd' <> metavar "FILE"
                                    <> help "File to write to instead of stdOut." <> value "")
    return Arguments{destination = case destination of [] -> Nothing; _ -> Just destination,
                     sizing = ProgramSize{len, algWidth, secWidth, flipWidth, outWidth, participants,
                                          bodyRatios = BodyRatios{compute, send, obliv},
                                          xorPreference, notFrequency, sendEagerness, oblivComplexity}}
  where metanat = metavar "NATURAL"
        metaname = metavar "NAME"
        metaprob = metavar "PROBABILITY"


main :: IO ()
main = do args <- getArgs
          let comment = "--  " ++ unwords args
          Arguments{destination, sizing} <- handleParseResult $
            execParserPure (prefs mempty) (info (argParser <**> helper) (progDesc "Generate a random .cho protocol.")) args
          q <- newStdGen
          let p = randomProgram sizing q
          let cho = pretty p
          let output = unlines [comment, "", cho, ""]
          let verb = maybe putStr writeFile destination
          verb output
