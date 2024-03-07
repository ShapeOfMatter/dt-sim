module HsSemantics where

import Data.Map.Strict (fromList, toList)
import Text.Parsec.String (parseFromFile)
import System.Environment (getArgs)
import Test.QuickCheck (arbitrary, vectorOf)
import Test.QuickCheck.Gen (generate)

import Choreography

main :: IO ()
main = do
  programFile : fixedInputs <- getArgs
  program' <- parseFromFile programParser programFile
  let program = either error id $ either (error . show) id $ validate mempty <$> program'
  let ProgramMetaData{inputVars, tapeVars} = metadata program
  let secrets = (toEnum . read @Int <$> fixedInputs) <> repeat True
  let inputs = Inputs $ fromList $ (snd <$> inputVars) `zip` secrets
  putStrLn $ "Inputs:  " ++ show (toList $ inputsMap inputs)
  tapes <- generate $ vectorOf (sum $ length <$> tapeVars) (arbitrary @Bool)
  putStrLn $ "Tapes:  " ++ show tapes
  let (os, vs) = deterministicEvaluation program inputs tapes
  putStrLn $ "Views:  " ++ show (toList $ toList <$> viewsMap vs)
  putStrLn $ "Outputs:  " ++ show (toList $ toList <$> outputsMap os)
