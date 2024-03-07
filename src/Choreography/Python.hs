{-# LANGUAGE QuasiQuotes #-}
module Choreography.Python where

import Data.Bool (bool)
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity(..))
import Data.List (intercalate, elemIndex, nub)
import Data.Map.Strict ((!), fromList, Map, toAscList, toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Set as Set
import Polysemy (Members, run, Sem)
import Polysemy.Writer (runWriter, tell, Writer)
import Polysemy.State (gets, modify, runState, State)

import Choreography.AbstractSyntaxTree
import Choreography.Metadata
import Choreography.Party
import Choreography.Semantics (Inputs, inputsMap, Outputs (Outputs), Tapes, Views(Views))
import Python
import Utils ((<$$>), litFile, pretty, Pretty1)

numpy :: String -> PythonExpression
numpy = ("np." ++)

n :: PythonExpression
n = "n"

header :: PythonLines
header = pythonLines [litFile|snippits/header.py|]


at :: (Proper f) => f Variable -> f Party -> [(Concrete Party, Variable)]
fv `at` fp = [(Identity $ value fp, value fv)]
ats :: (Proper f) => f Variable -> PartySet -> Map (Concrete Party) [Variable]
fv `ats` ps = fromList [(Identity p, [value fv]) | p <- Set.toList . parties $ ps]
registrationIndex :: (Members '[State ProgramMetaData] r) =>
                     (ProgramMetaData -> [(Concrete Party, Variable)]) -> ProgramMetaData -> Sem r Int
registrationIndex func meta = do modify (<> meta)
                                 gets $ (-1 +) . length . func

flattenIndicesM :: Map p [Variable] -> [String]
flattenIndicesM mpvs = nub . concat $ fmap variable . snd <$> toAscList mpvs
flattenIndicesL :: [(p, Variable)] -> [String]
flattenIndicesL mpvs = variable . snd <$> mpvs

pySemantics :: forall f r.
             (Members '[
                 Writer PythonLines
                ,State ProgramMetaData
               ] r,
              Proper f, Functor f, Traversable f, Pretty1 f) =>
             Program f -> Sem r ()
pySemantics = traverse_ pyStatement

pyStatement :: forall f r.
             (Members '[
                 Writer PythonLines
                ,State ProgramMetaData
               ] r,
              Proper f, Functor f, Traversable f, Pretty1 f) =>
             f (Statement f) -> Sem r ()
pyStatement fs = do
  case value fs of
    (Compute fv falg) -> bindVar fv $ pyAlg falg
    (Secret fv fp) -> do i <- registrationIndex inputVars $ mempty{inputVars = fv `at` fp}
                         bindVar fv $ "inputs" `index` [show i]
    (Flip fv fp) -> do i <- registrationIndex tapeVars $ mempty{tapeVars = fv `at` fp, viewVars= fromList $ (:[]) <$$> (fv `at` fp)}
                       bindVar fv $ "tapes" `index` [show i]
    (Send fps fv) -> do modify (<> mempty{viewVars = fv `ats` value fps})
                        tell . comment . pretty $ value fs
    (Output fv) -> do modify (<> mempty{outputVars = fv `ats` owners fv})
                      tell . comment . pretty $ value fs
    (Oblivious fv fps fbody) -> do modify (<> mempty{viewVars = fv `ats` value fps})
                                   bindVar fv $ pyObliv $ value fbody
  where bindVar fv body = tell $ pythonLines $ variable (value fv) ++ " = " ++ body

pyAlg :: (Proper f) => f (Algebra f) -> PythonExpression
pyAlg = pyAlg' . value
  where pyAlg' (Literal fb) = numpy (bool "zeros" "ones" $ bit $ value fb) `apply` [n]
        pyAlg' (Var fv) = variable . value $ fv
        pyAlg' (Xor fa1 fa2) = "xor" `apply` [pyAlg fa1, pyAlg fa2]
        pyAlg' (And fa1 fa2) = "land" `apply` [pyAlg fa1, pyAlg fa2]
        pyAlg' (Not falg) = "xor" `apply` [pyAlg falg, numpy "ones" `apply` [n]]

pyObliv :: (Proper f) => ObvBody f -> PythonExpression
pyObliv (ObvBody fc0 fc1 fv) = numpy "choose" `apply` [variable $ value fv, "" `apply` [obvChoice $ value fc0, obvChoice $ value fc1]]
  where obvChoice (ObvLeaf var) = variable var
        obvChoice (ObvBranch body) = pyObliv body


data PyTrace = PyTrace { inputs :: [Int], tapes :: [Int], outputs :: [Int], views :: [Int] } deriving (Eq, Ord, Read, Show)

asPythonFunction :: (Proper f, Functor f, Traversable f, Pretty1 f) =>
                    Program f -> (PythonLines, ProgramMetaData)
asPythonFunction prog = (pythonLines [litFile|snippits/procedure_signature.py|]
                         <> indent 1 pyLines
                         <> indent 1 gather
                         <> pythonLines [litFile|snippits/procedure_return.py|]
                         , pmd)
  where (pyLines, (pmd@ProgramMetaData{inputVars, tapeVars, viewVars, outputVars}, ()))
          = run . runWriter . runState mempty $ pySemantics prog
        gather = pythonLines $ "selected_inputs = [" ++ intercalate "," (flattenIndicesL inputVars) ++ "]\n\
                               \selected_tapes = [" ++ intercalate "," (flattenIndicesL tapeVars) ++ "]\n\
                               \selected_views = [" ++ intercalate "," (flattenIndicesM viewVars) ++ "]\n\
                               \selected_outputs = [" ++ intercalate "," (flattenIndicesM outputVars) ++ "]"

asPythonProgram :: (Proper f, Functor f, Traversable f, Pretty1 f) => Program f -> Inputs -> Tapes -> (PythonLines, ProgramMetaData)
asPythonProgram prog ins tps = (header
                                <> function
                                <> setup
                                <> pythonLines [litFile|snippits/footer.py|]
                               , pmd)
  where (function, pmd@ProgramMetaData{inputVars, tapeVars}) = asPythonFunction prog
        setup = pythonLines $ "inputs = np.array([" ++ intercalate ", " [pretty $ inputsMap ins ! var | (_, var) <- inputVars] ++ "])\n\
                              \tapes = np.array([" ++ intercalate ", " [pretty t | (t, _) <- tps `zip` tapeVars] ++ "])"

runPythonProgram :: (Proper f, Functor f, Traversable f, Pretty1 f) => Program f -> Inputs -> Tapes -> IO (Outputs, Views, PythonLines)
runPythonProgram prog ins tps = do PyTrace{outputs, views} <- runPythonCommand @PyTrace code
                                   return (Outputs $ outputs `arrange` outputVars, Views . ((:[]) <$$>) $ views `arrange` viewVars, code)
  where (code, ProgramMetaData{outputVars, viewVars}) = asPythonProgram prog ins tps
        arrange :: [Int] -> Map (Concrete Party) [Variable] -> Map (Concrete Party) (Map Variable Bool)
        vals `arrange` mapping = let vars = Variable <$> flattenIndicesM mapping
                                 in fromList . ((\v -> (v, toEnum $ vals !! fromJust (elemIndex v vars))) <$>) <$> mapping

decisionTreeTest :: (Proper f, Functor f, Traversable f, Pretty1 f) => PartySet -> Int -> Int -> Int -> Program f -> (PythonLines, ProgramMetaData)
decisionTreeTest ps treesN trainingN testingN prog = (header
                                                      <> function
                                                      <> pythonLines [litFile|snippits/dtree.py|]
                                                      <> call
                                                     , pmd)
  where (function, pmd@ProgramMetaData{inputVars, tapeVars, viewVars, outputVars}) = asPythonFunction prog
        call = pythonLines $ "perform_test(" ++ intercalate ", " [
            "[" ++ intercalate ", " [if runIdentity p `isElementOf` ps then corrupt else honest | (p, _) <- inputVars] ++ "]",
            show $ length tapeVars,
            "[" ++ intercalate ", " [if Variable var `looksCorruptIn` viewVars then corrupt else honest | var <- flattenIndicesM viewVars] ++ "]",
            "[" ++ intercalate ", " [if Variable var `looksCorruptIn` outputVars then corrupt else honest | var <- flattenIndicesM outputVars] ++ "]",
            show trainingN,
            show testingN,
            show treesN
          ] ++ ")"
        (corrupt, honest) = ("False", "True")
        var `looksCorruptIn` mapping = isJust $ ps `intersect` (
                 Parties . Set.fromList . fmap (runIdentity . fst) . toList . Map.filter (elem var) $ mapping
            )

runDTreeTest :: (Proper f, Functor f, Traversable f, Pretty1 f) => PartySet -> Int -> Int -> Int -> Program f -> IO Double
runDTreeTest ps treesN trainingN testingN prog = runPythonCommand @Double code
  where (code, _) = decisionTreeTest ps treesN trainingN testingN prog
