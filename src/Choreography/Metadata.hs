module Choreography.Metadata where

import Control.Arrow (Arrow (first))
import Data.Functor.Identity (Identity(..))
import Data.Map.Strict ((!?), fromList, insert, Map, mapKeysWith, unionWith, insert, singleton)
import Data.Maybe (fromMaybe)
import Data.Set (toList)
import Polysemy (Members, run, Sem)
import Polysemy.Writer (runWriter, tell, Writer)
import Polysemy.State (gets, modify, evalState, State)

import Choreography.AbstractSyntaxTree
import Choreography.Functors (owners, Proper, value)
import Choreography.Party hiding (insert, singleton)
import Utils (pretty, Pretty1)


data ProgramMetaData = ProgramMetaData { inputVars :: [(Concrete Party, Variable)]
                                       , tapeVars :: [(Concrete Party, Variable)]
                                       , viewVars :: Map (Concrete Party) (Map Variable Int)
                                       , outputVars :: Map (Concrete Party) [Variable]
                                       } deriving (Eq, Ord, Show)
instance Semigroup ProgramMetaData where
  ProgramMetaData ivs1 tvs1 vvs1 ovs1 <>  ProgramMetaData ivs2 tvs2 vvs2 ovs2 =
    ProgramMetaData (ivs1 <> ivs2) (tvs1 <> tvs2) (unionWith (unionWith (+)) vvs1 vvs2) (unionWith (<>) ovs1 ovs2)
instance Monoid ProgramMetaData where
  mempty = ProgramMetaData mempty mempty mempty mempty

dealiasMeta :: Map Party (Concrete Party) -> ProgramMetaData -> ProgramMetaData
dealiasMeta m ProgramMetaData{inputVars, tapeVars, viewVars, outputVars} =
    ProgramMetaData{ inputVars = first da <$> inputVars
                  , tapeVars = first da <$> tapeVars
                  , viewVars = mapKeysWith (<>) da viewVars
                  , outputVars = mapKeysWith (<>) da outputVars }
  where da = dealias m . runIdentity

metadata :: forall f.
            (Functor f, Proper f, Pretty1 f) =>
            Program f -> ProgramMetaData
metadata = fst . run . runWriter . evalState mempty . traverse mdStatement

mdStatement :: forall f r.
               (Members '[Writer ProgramMetaData,
                          State (Map FuncName ([Party], ProgramMetaData))] r,
                Functor f, Proper f, Pretty1 f) =>
               f (Statement f) -> Sem r ()
mdStatement = (\case
    Compute _ _ ->  return ()
    Secret var p -> tell mempty{ inputVars = [(Identity $ value p, value var)] }
    Flip var p -> tell mempty{ tapeVars = [(Identity $ value p, value var)]
                             , viewVars = singleton (Identity $ value p) (singleton (value var) 1) }
    Send p2s var -> tell mempty{ viewVars = fromList [(Identity p, singleton (value var) 1) | p <- toList . parties . value $ p2s] }
    Output var -> tell mempty{ outputVars = fromList [(Identity p, [value var]) | p <- toList . parties . owners $ var] }
    Oblivious var p2s _ -> tell mempty{ viewVars = fromList [(Identity p, singleton (value var) 1) | p <- toList . parties . value $ p2s] }
    Declaration fname pargs body -> do let funcParties = value . fst <$> pargs
                                       let metaD = metadata body
                                       modify $ insert (value fname) (funcParties, metaD)
    Call fname pargs _ -> do (funcParties, metaD) <- fromMaybe (error $ "Couldn't find function " ++ pretty fname ++ ".") <$> gets (!? value fname)
                             let aliases = fromList [(funcParty, Identity $ value callParty) | (funcParty, (callParty, _)) <- funcParties `zip` pargs]
                             tell $ dealiasMeta aliases metaD
  ) . value
