module TPython where

import qualified Data.Set as Set
import Data.Word (Word64)
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck (ioProperty)
import Text.Parsec (runParser)

import Choreography
import Python
import Utils (throwsException)

tests :: IO [Test]
tests = do return [ testHealth
                  , antiHealth
                  --, exampleFromFile
                  --, gmwAndGates
                  , insecureProg
                  , secureProg]


testHealth :: Test
testHealth = testProperty "Health Check" $ ioProperty $ healthCheck [PythonLibrary "numpy", PythonLibrary "sklearn"]

antiHealth :: Test
antiHealth = testProperty "Healthcheck could actually fail"
  $ ioProperty
  $ throwsException @IOError
  $ healthCheck [PythonLibrary "asdfasfasdfasdfjjj"]


insecureProg :: Test
insecureProg = testProperty "Insecure program gives low p-value." $ ioProperty do
    pValue <- experiment_ @Word64 IterConfig{iterations=20, trainingN=64, testingN=64} program (Parties $ Set.singleton p2)
    return . not $ pValue `indicatesSecurityBy` PValue 0.01
  where program :: Program Located
        Right program' =  runParser programParser mempty "hardcode example" "\
          \rand = FLIP @P1\n\
          \sec = SECRET @P1\n\
          \SEND rand TO P2\n\
          \SEND sec TO P2\n\
          \OUTPUT rand"
        Right program = validate mempty program'

secureProg :: Test
secureProg = testProperty "Secure program gives a random p-value. (May randomly fail sometimes!)" $ ioProperty do
    pValue <- experiment_ @Word64 IterConfig{iterations=20, trainingN=64, testingN=64} program (Parties $ Set.singleton p2)
    return $ pValue `indicatesSecurityBy` PValue 0.01
  where program :: Program Located
        Right program' = runParser programParser mempty "hardcode example" "\
          \rand = FLIP @P1\n\
          \sec = SECRET @P1\n\
          \SEND rand TO P2\n\
          \OUTPUT rand"
        Right program = validate mempty program'

