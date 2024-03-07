module Python where

import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Data.List (intercalate)
import Data.Foldable (Foldable(fold))
import GHC.IO.Handle
import GHC.IO.Handle.Text (hPutStrLn)
import System.Exit
import System.Process
import Text.Read (readMaybe)

runPythonCommand :: forall a. (Read a) => PythonLines -> IO a
runPythonCommand code = withCreateProcess
  (shell "python"){std_in=CreatePipe, std_err=Inherit, std_out=CreatePipe}
  (\(Just stdin_hl) (Just stdout_hdl) Nothing ph -> do
     output <- hGetContents stdout_hdl
     hPutStrLn stdin_hl $ asString code
     hClose stdin_hl
     exitCode <- waitForProcess ph
     evaluate $ rnf output  -- Unclear if this is actually doing anything.
     case exitCode of
       ExitSuccess -> maybe (ioError $ userError $ "Couldn't parse " ++ show output) return $ readMaybe @a output
       failure@(ExitFailure _) -> ioError $ userError $ "Python terminated with " ++ show failure
  )

newtype Library = PythonLibrary {library :: String} deriving (Eq, Ord, Show)
importLib :: Library -> PythonLines
importLib (PythonLibrary l) = pythonLines $ "import " ++ l

healthCheck :: [Library] -> IO ()
healthCheck libs = do let code = fold (importLib <$> libs) <> pythonLines "print(5)"
                      value <- runPythonCommand @Int code
                      case value of
                        5 -> return ()
                        _ -> ioError $ userError "Unknown problem with your python environment. Hopefully this isn't the only error message?"

type PythonExpression = String
newtype PythonLines = PythonLines_{_asStrings :: [PythonExpression]} deriving (Eq, Monoid, Ord, Semigroup, Show)
pythonLines :: String -> PythonLines
pythonLines = PythonLines_ . lines
asString :: PythonLines -> String
asString = unlines . _asStrings

apply :: PythonExpression -> [PythonExpression] -> PythonExpression
f `apply` args = f ++ "(" ++ intercalate ", " args ++ ")"
index :: PythonExpression -> [PythonExpression] -> PythonExpression
f `index` args = f ++ "[" ++ intercalate ", " args ++ "]"

comment :: String -> PythonLines
comment c = pythonLines $ "# " ++ c

indent :: Int -> PythonLines -> PythonLines
indent level (PythonLines_ ls) = PythonLines_ $ (replicate (4 * level) ' ' ++ ) <$> ls
