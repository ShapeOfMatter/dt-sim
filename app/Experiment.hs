module Experiment where

import Control.DeepSeq (NFData(rnf))
import Control.Exception (catch, SomeException)
import Control.Monad (foldM)
import qualified Data.ByteString.Lazy as ByteString
import Data.Csv (encode)
import Data.List (isInfixOf, transpose, groupBy, uncons)
import Data.Function (on)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time.Clock (diffUTCTime, getCurrentTime, NominalDiffTime, nominalDiffTimeToSeconds)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word64)
import GHC.IO (evaluate)
import GHC.Exts (fromList)
import Numeric.Natural (Natural)
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
                           , strOption
                           , value
                           )
import System.Environment (getArgs)
import System.Random (newStdGen)
import Text.Read (readMaybe)

import Choreography hiding (value, writeCSV)
import Utils ((<$$>), (<$$$>))

type Logger = String -> IO ()

data Settings = Settings { sizing :: ProgramSize
                         , iters :: [IterConfig]
                         , alpha :: PValue
                         , screen :: Maybe IterConfig
                         , truncatePower :: Maybe Natural
                         } deriving (Read, Show)

data Arguments = Arguments { settingsFile :: FilePath
                           , destination :: FilePath
                           , filePrefix :: String
                           , save :: Natural
                           , totalGenerated :: Natural
                           }

argParser :: Parser Arguments
argParser = do
    settingsFile <- strOption (     long "settings"       <> short 's' <> metavar "PATH"
                                    <> help "A .settings file from which to `read` the `Settings` object.")
    destination <- strOption (      long "destination"       <> short 'd' <> metavar "PATH"
                                    <> help "Directory in which to write passing protocols.")
    filePrefix <- strOption (       long "prefix"            <> short 'p' <> metavar "WORD" <> value ""
                                    <> help "Prefix for sequential filenames, to distinguish between runs.")
    save <- option auto (           long "save-n"          <> short 'w' <> metanat <> value (-1 :: Int)
                                    <> help "How many examples to save to disk. (Negatives wrap from n+1.)")
    totalGenerated <- option auto ( long "generate"          <> short 'n' <> metanat
                                    <> help "The total number of programs to generate and test.")
    return Arguments{settingsFile, destination,
                     filePrefix = case filePrefix of [] -> ""; fp -> fp ++ "_",
                     save = fromIntegral if save < 0 then fromIntegral totalGenerated + 1 + save else fromIntegral save,
                     totalGenerated}
  where metanat = metavar "NAT"

main :: IO ()
main = do args <- getArgs
          Arguments{settingsFile, destination, filePrefix, save, totalGenerated} <- handleParseResult $
            execParserPure (prefs mempty)
                           (info (argParser <**> helper)
                                 (progDesc $ "Randomly generate .cho protocols and check their security"
                                           ++ " using the provided dtree settings.")) args
          settings@Settings{iters, alpha} <- fromMaybe (error "Couldn't parse settings file.") . readMaybe <$> readFile settingsFile
          time <- getCurrentTime
          let settingsName = takeWhile (/= '.') . last . groupBy ((&&) `on` (/= '/')) $ settingsFile
          let runName = filePrefix ++ settingsName ++ formatTime defaultTimeLocale "_%04Y_%b_%d_%H_%M_%S_" time
          let fileNames = [(i <= save, destination ++ "/" ++ runName ++ show i ++ ".cho")
                           | i :: Natural <- [1..totalGenerated]]
          let writeLog = appendFile $ destination ++ "/" ++ runName ++ "log.txt"
          writeLog $ show settings ++ "\n"
          results <- catMaybes <$> mapM (blindDetermination writeLog . uncurry (attempt writeLog settings)) fileNames
          writeSankey (destination ++ "/" ++ runName ++ ".sankey.txt")
            . sankey (`indicatesSecurityBy` alpha)
            . zip (testName <$> iters)
            . transpose
            . (fromMaybe (PValue 0) <$$>)
            . (fst <$$$>)
            . (snd <$>)
            $ results
          writeCSV (destination ++ "/" ++ runName ++ ".csv") (`indicatesSecurityBy` alpha) (testName <$> iters) results

attempt :: Logger -> Settings -> Bool -> FilePath -> IO (String, [Maybe (PValue, NominalDiffTime)])
attempt writeLog settings@Settings{sizing, iters, screen, alpha} write destination = do
  cho <- preScreenGeneration screen alpha sizing
  writeLog $ "Generated " ++ show (length cho) ++ " line program. "
  pvals <- foldM (doTests writeLog settings cho) [] iters
  if write
    then do writeFile destination $ unlines [makeHeader sizing (iters `zip` (fst <$$> pvals)),
                                             render cho]
            writeLog $ "Wrote out to \"" ++ destination ++ "\"."
    else writeLog "Discarding. "
  return (destination, pvals)

doTests :: Logger -> Settings -> Program Located -> [Maybe (PValue, NominalDiffTime)] -> IterConfig -> IO [Maybe (PValue, NominalDiffTime)]
doTests writeLog Settings{alpha, truncatePower} cho acc iter = (acc ++) . (:[]) <$>
  if keepGoing truncatePower $ uncons . reverse $ acc
    then do evaluate $ rnf iter
            evaluate $ rnf cho
            evaluate $ rnf corruption
            t1 <- getCurrentTime
            pval <- experiment_ @Word64 iter cho corruption
            evaluate $ rnf pval
            t2 <- getCurrentTime
            writeLog $ "Measured p-value: " ++ show pval ++ " "
            return $ Just (pval, diffUTCTime t2 t1)
    else pure Nothing
  where keepGoing :: Maybe Natural -> Maybe (Maybe (PValue, NominalDiffTime), [Maybe (PValue, NominalDiffTime)]) -> Bool
        keepGoing _         Nothing                          = True   -- uncons gives Nothing for []
        keepGoing Nothing   _                                = True   -- if no truncatePower, allways do all iters
        keepGoing _         (Just (Nothing,              _)) = False  -- if we skipped last time, keep skipping
        keepGoing (Just tp) (Just (Just (mostRecent, _), _)) = mostRecent `indicatesSecurityBy` PValue (pvalue alpha ^ tp)

preScreenGeneration :: Maybe IterConfig -> PValue -> ProgramSize -> IO (Program Located)
preScreenGeneration screen alpha sizing =
  do q <- newStdGen
     let cho = either error id . validate mempty . snd . fakePos 0 $ randomProgram sizing q
     use <- case screen of Nothing -> pure True
                           Just test -> (`indicatesSecurityBy` alpha) <$> experiment_ @Word64 test cho corruption
     if use then return cho
            else preScreenGeneration screen alpha sizing

blindDetermination :: Logger -> IO a -> IO (Maybe a)
blindDetermination writeLog task = do retval <- catch (Just <$> task)
                                                      \(e :: SomeException) -> do
                                                          let m = show e
                                                          writeLog if "ValueError: Found array with 0 feature(s)" `isInfixOf` m
                                                                     then "Ignoring un-assessable protocol."
                                                                     else m
                                                          return Nothing
                                      writeLog "\n"
                                      return retval

corruption :: PartySet
corruption = Parties $ fromList [corrupt, p1]

makeHeader :: ProgramSize -> [(IterConfig, Maybe PValue)] -> String
makeHeader sizing tests = unlines [ "{-"
                                       , show sizing
                                       , show tests
                                       , "-}"
                                       ]

type SecurityJudgment = PValue -> Bool

testName :: IterConfig -> String
testName IterConfig{iterations, trainingN, testingN} = "test_" ++ show iterations ++ "_" ++ show trainingN ++ "_" ++ show testingN


sankey :: SecurityJudgment -> [(String, [PValue])] -> [(String, Int, String)]
sankey isSec pvals = let thresholded = isSec <$$$> pvals
                         transitions = [ (name1, name2, [\(b1, b2) -> (b1 == v1) && (b2 == v2) | (v1, v2) <- vals1 `zip` vals2])
                                         | ((name1, vals1), (name2, vals2)) <- thresholded `zip` tail thresholded ]
                         bucketName pass = if pass then ("SEC_" ++) else ("INS_" ++)
                         generations = [("Generated"
                                        ,length . filter (== pass) . snd . head $ thresholded
                                        ,(bucketName pass . fst . head) thresholded)
                                        | pass <- [True, False]]
                     in generations ++ do (name1, name2, ts) <- transitions
                                          pass1 <- [True, False]
                                          pass2 <- [True, False]
                                          return (bucketName pass1 name1
                                                 ,length $ filter ($ (pass1, pass2)) ts
                                                 ,bucketName pass2 name2)

writeSankey :: FilePath -> [(String, Int, String)] -> IO ()
writeSankey f = writeFile f . unlines . (format <$>)
  where format (name1, quantity, name2) = name1 ++ "[" ++ show quantity ++ "]" ++ name2

writeCSV :: FilePath -> SecurityJudgment -> [String] -> [(String, [Maybe (PValue, NominalDiffTime)])] -> IO ()
writeCSV f isSec testNames results = ByteString.writeFile f . encode $ header : body
  where header = "filename" : concat [ [name ++ "_time", name ++ "_p", name ++ "_secure"]
                                       | name <- testNames]
        body = [ filename : concatMap triplet  ps
                 | (filename, ps) <- results]
        triplet Nothing = ["NA", "NA", show . fromEnum $ False]
        triplet (Just (p, ndt)) = [show $ nominalDiffTimeToSeconds ndt, show p, show . fromEnum . isSec $ p]
