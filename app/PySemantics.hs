module PySemantics where

import Data.Map.Strict (fromList, toList)
import Text.Parsec.String (parseFromFile)
import System.Environment (getArgs)
import Test.QuickCheck (arbitrary, vectorOf)
import Test.QuickCheck.Gen (generate)

import Choreography
import Utils
import Python

main :: IO ()
main = do
  programFile : fixedInputs <- getArgs
  program' <- parseFromFile (changeState (const ()) (const mempty) programParser) programFile
  let program = either error id $ either (error . show) id $ validate mempty <$> program'
  let (_, ProgramMetaData{inputVars, tapeVars}) = asPythonFunction program
  let secrets = (toEnum . read @Int <$> fixedInputs) <> repeat True
  let inputs = Inputs $ fromList $ (snd <$> inputVars) `zip` secrets
  putStrLn $ "Inputs:  " ++ show (toList $ inputsMap inputs)
  tapes <- generate $ vectorOf (sum $ length <$> tapeVars) (arbitrary @Bool)
  putStrLn $ "Tapes:  " ++ show tapes
  (os, vs, code) <- runPythonProgram program inputs tapes
  writeFile ".temp.py" $ asString code
  putStrLn $ "Views:  " ++ show (toList $ toList <$> viewsMap vs)
  putStrLn $ "Outputs:  " ++ show (toList $ toList <$> outputsMap os)
