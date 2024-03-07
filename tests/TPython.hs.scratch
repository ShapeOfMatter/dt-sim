module TPython where

import Data.Map.Strict (fromList, toList)
import qualified Data.Set as Set
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck (arbitrary, Gen, ioProperty, Property, vectorOf, counterexample, expectFailure)
import Text.Parsec (runParser)
import Text.Parsec.String (parseFromFile)

import Choreography
import Python
import Utils (changeState, throwsException)

tests :: IO [Test]
tests = do return [ testHealth
                  , antiHealth
                  , exampleFromFile
                  , gmwAndGates
                  , insecureProg
                  , secureProg]


testHealth :: Test
testHealth = testProperty "Health Check" $ ioProperty $ healthCheck [PythonLibrary "numpy", PythonLibrary "sklearn"]

antiHealth :: Test
antiHealth = testProperty "Healthcheck could actually fail"
  $ ioProperty
  $ throwsException @IOError
  $ healthCheck [PythonLibrary "asdfasfasdfasdfjjj"]

exampleFromFile :: Test
exampleFromFile = testProperty "Run examples/dt_test.py" $ ioProperty do
  code <- pythonLines <$> readFile "examples/dt_test.py"
  (idealScore, realScore) <- runPythonCommand @(Double, Double) code
  return $ abs (0.5 - idealScore) < 0.15 && abs (0.5 - realScore) < 0.15

gmwAndGates :: Test
gmwAndGates = testProperty "Two party three-arg AND in GMW (python)" $ ioProperty gmwAndGatesIO
gmwAndGatesIO :: IO (Gen Property)
gmwAndGatesIO = do
  program' <- parseFromFile (changeState (const ()) (const mempty) programParser) "examples/3party2andGMW.cho"
  let program = either error id $ either (error . show) id $ validate mempty <$> program'
  return do  -- The Gen Monad!
    secrets <- vectorOf 3 (arbitrary @Bool)
    let inputs = Inputs $ fromList $ [Variable "c_in"
                                     ,Variable "h1_in"
                                     ,Variable "h2_in"] `zip` secrets
    tapes <- vectorOf 5 (arbitrary @Bool)
    let (outputs, views) = deterministicEvaluation program inputs tapes
    return $ ioProperty do -- Back in the IO monad!?
      (os, vs, code) <- runPythonProgram program inputs tapes
      let messages = [ asString code,
                      "inputs = " ++ show (toList $ inputsMap inputs),
                      "tapes = " ++ show tapes,
                      "views = " ++ show (concat $ toList <$> viewsMap vs),
                      "outputs = " ++ show (concat $ toList <$> outputsMap os),
                      "expected views = " ++ show (concat $ toList <$> outputsMap outputs)]
      return $ counterexample (unlines messages) $ os == outputs && vs == views

insecureProg :: Test
insecureProg = testProperty "Insecure program gives low p-value." $ ioProperty do
    pValue <- runDTreeTest (Parties $ Set.singleton p2) 14 100 100 program
    return $ pValue < 0.05
  where program :: Program Located
        Right program' =  runParser programParser mempty "hardcode example" "\
          \rand = FLIP @P1\n\
          \sec = SECRET @P1\n\
          \SEND rand TO P2\n\
          \SEND sec TO P2\n\
          \OUTPUT rand"
        Right program = validate mempty program'

secureProg :: Test
secureProg = testProperty "Secure program gives a random p-value." $ expectFailure $ ioProperty do
    pValue <- runDTreeTest (Parties $ Set.singleton p2) 14 100 100 program
    return $ pValue < 0.05
  where program :: Program Located
        Right program' = runParser programParser mempty "hardcode example" "\
          \rand = FLIP @P1\n\
          \sec = SECRET @P1\n\
          \SEND rand TO P2\n\
          \OUTPUT rand"
        Right program = validate mempty program'
