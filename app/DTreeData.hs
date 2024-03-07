module DTreeData where

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Word (Word64)
import Options.Applicative ( (<**>)
                           , argument
                           , auto
                           , help
                           , helper
                           , info
                           , long
                           , metavar
                           , Parser
                           , progDesc
                           , short
                           , strOption
                           , switch
                           , value
                           , execParser
                           )
import Text.Parsec.String (parseFromFile)

import Choreography hiding (source, value)
import Utils

data Arguments = Arguments { iters :: IterConfig
                           , source :: Maybe FilePath
                           , performTest :: Bool
                           }

argParser :: Parser Arguments
argParser = do
    iterations <- argument auto (   metavar "ITERATIONS" <> help "How many times the cycle to testing and training a decision tree will repeat.")
    trainingN <- argument auto (    metavar "TRAINING"   <> help "How many rows of data to train the decision trees on.")
    testingN <- argument auto (     metavar "TESTING"    <> help "How many rows of data to test the decision trees on.")
    source <- strOption (           long "file"          <> short 'f' <> metavar "FILENAME"
                                                         <> help "File to read from instead of stdIn." <> value "")
    performTest <- switch (         short 'p'            <> long "perform-test"
                                                         <> help ("INSTEAD of printing the data to stdOut, send it directly to python,"
                                                                  ++ " and print the p-value."))
    return Arguments{iters = IterConfig {iterations, trainingN, testingN },
                     source = case source of [] -> Nothing; _ -> Just source,
                     performTest}

main :: IO ()
main = do
  let corruption = Parties $ Set.fromList [corrupt, p1]
  Arguments{iters, source, performTest} <- execParser $
      info (argParser <**> helper) (progDesc $ unlines [
        "Generate ITERATIONS * (TRAINING + TESTING) rows of trace data for consumption by the d-tree system.",
        "assumes " ++ pretty corruption ++ " are corrupt."])
  -- internet suggets easy way of optionally reading from stdin:
  -- https://unix.stackexchange.com/questions/483573/a-command-wants-file-paths-how-can-i-give-it-stdin-for-the-infile-and-stdout
  program' <- parseFromFile (changeState (const ()) (const mempty) programParser) $ fromMaybe "/dev/stdin" source
  let program = either error id $ either (error . show) id $ validate mempty <$> program'
  if performTest
    then experiment_ @Word64 iters program corruption >>= print
    else printParallelized @Word64 iters program corruption

