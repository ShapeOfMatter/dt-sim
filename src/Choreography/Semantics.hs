module Choreography.Semantics
where

import Control.Monad (unless, when)
import Data.Bits ((.&.), (.|.), complement, FiniteBits, xor, zeroBits)
import Data.Bifunctor (first, second)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Functor.Identity (Identity(..))
import Data.Map.Strict ((!?), empty, insert, Map, singleton, unionWith, toList, fromList)
import Data.Maybe (fromMaybe, fromJust)
import Polysemy (Members, run, runM, Sem)
import Polysemy.Input (Input, input)
import Polysemy.Reader (asks, Reader, runReader)
import Polysemy.State (get, gets, modify, put, runState, State)
import Polysemy.Trace (Trace, trace, traceToStdout)
import Polysemy.Writer (runWriter, tell, Writer)

import Choreography.AbstractSyntaxTree
import Choreography.Functors (owners, Located, Proper, value)
import Choreography.Party (Concrete, dealias, dealiass, intersect, isElementOf, Party(..), PartySet(..))
import qualified Choreography.Party as Pty
import Utils ((<$$>), (<$$$>), Pretty, pretty, Pretty1, prettyf, runInputUnsafe)

class NS ns a | ns -> a where
  find :: ns -> Variable -> Maybe a
  bind :: Variable -> a -> ns -> ns

type Semanticable w = (FiniteBits w, Show w, Pretty w)

data EvalState f w = EvalState { varContext :: Map Variable (PartySet, w)
                               , funcContext :: Map FuncName ([(Party, [Variable])], Program f)
                               , aliases :: Map Party (Concrete Party)
                               }
deriving instance (forall a. (Show a) => Show (f a), Semanticable w) => Show (EvalState f w)
deriving instance Functor (EvalState f)
instance forall f w. Semigroup (EvalState f w) where
  EvalState vc1 fc1 a1 <> EvalState vc2 fc2 a2 = EvalState (vc1 <> vc2) (fc1 <> fc2) (a1 <> a2)
instance forall f w. Monoid (EvalState f w) where
  mempty = EvalState mempty mempty mempty
instance forall f w. NS  (EvalState f w) (PartySet, w) where
  find = (!?) . varContext
  bind v a es@EvalState{varContext} = es{ varContext = insert v a varContext }
instance forall f. (Pretty1 f) => Pretty1 (EvalState f) where
  prettyf EvalState{varContext, funcContext, aliases} = unlines
    $ ( (\(var, (ps, b)) -> "  " ++ pretty var ++ ": " ++ b ++ " @ " ++ pretty ps)  <$> toList varContext )
    <> ( (\(fName, (args, _)) -> "  " ++ pretty fName ++ prettyArgsList (Identity <$$$> (first Identity <$> args))) <$> toList funcContext )
    <> ( (\(plocal, preal) -> "  " ++ pretty plocal ++ " -> " ++ pretty preal) <$> toList aliases )

basicallyEqual :: (Eq w) => EvalState f w -> EvalState f w -> Bool
basicallyEqual = (==) `on` \EvalState{varContext, funcContext, aliases} -> (varContext, fst <$> funcContext, aliases)

newtype Inputs w = Inputs { inputsMap :: Map Variable w } deriving (Eq, Monoid, Semigroup, Show)
instance NS (Inputs w) w where
  find = (!?) . inputsMap
  bind v a = Inputs . insert v a . inputsMap

{-newtype Tapes = Tapes { tapesMap :: Map Variable Bool } deriving (Eq, Monoid, Semigroup, Show)
instance NS Tapes Bool where
  find = (!?) . tapesMap
  bind v a = Tapes . insert v a . tapesMap-}
type Tapes w = [w]

newtype Outputs w = Outputs { outputsMap :: Map (Concrete Party) (Map Variable w) } deriving (Eq, Show)
deriving instance Functor Outputs
instance Semigroup (Outputs w) where
  Outputs om1 <> Outputs om2 = Outputs $ unionWith (<>) om1 om2
instance Monoid (Outputs w) where mempty = Outputs empty
instance Pretty1 Outputs where
  prettyf (Outputs m) = pretty . (second (pretty . toList) <$>) . toList $ m

newtype Views w = Views { viewsMap :: Map (Concrete Party) (Map Variable [w]) } deriving (Eq, Show)
deriving instance Functor Views
instance Semigroup (Views w) where
  Views om1 <> Views om2 = Views $ unionWith (unionWith (<>)) om1 om2
instance Monoid (Views w) where mempty = Views empty
instance Pretty1 Views where
  prettyf (Views m) = pretty . (second (pretty . toList) <$>) . toList $ m


-- Unsafe Lookup
uslkup :: forall ns f a.
          (NS ns a,
           Functor f, Proper f, Pretty1 f) =>
          f Variable -> ns -> a
fv `uslkup` ns = fromMaybe
  (error $ "Free variable " ++ pretty fv ++ " appeared during evaluation.")
  (ns `find` value fv)


semantics :: forall w f r.
             (Members '[
               Reader (Inputs w),
               Input w,
               Writer (Outputs w),
               Writer (Views w),
               State (EvalState f w)
               ] r,
              Proper f, Functor f, Traversable f, Pretty1 f,
              Semanticable w) =>
             Program f -> Sem r ()
semantics = traverse_ stmtSemantics

verboseSemantics :: forall w r.
                    (Members '[
                      Reader (Inputs w),
                      Input w,
                      Writer (Outputs w),
                      Writer (Views w),
                      State (EvalState Located w),
                      Trace
                      ] r,
                     Semanticable w) =>
                    Program Located -> Sem r ()
verboseSemantics = traverse_ sem
  where sem st = do trace $ pretty $ fst st
                    stmtSemantics st
                    est <- get
                    trace $ pretty est

stmtSemantics :: forall w f r.
                 (Members '[
                   Reader (Inputs w),
                   Input w,
                   Writer (Outputs w),
                   Writer (Views w),
                   State (EvalState f w)
                   ] r,
                  Proper f, Functor f, Traversable f, Pretty1 f,
                  Semanticable w) =>
                 f (Statement f) -> Sem r ()
stmtSemantics fs = case value fs of
    Compute var falg -> do val <- algSemantics (value falg)
                           modify $ bind (value var) val
    Secret var _ -> do val <- asks @(Inputs w) $ uslkup var
                       modify $ bind (value var) (owners var, val)
    Flip var _ -> do val <- input
                     modify $ bind (value var) (owners var, val)
                     recordViews (value var) val (owners var)
    Send p2s var -> do (p1s, val) <- gets $ uslkup var
                       recordViews (value var) val (value p2s)
                       modify $ bind (value var) (p1s <> value p2s, val)
    Output var -> do (ps, val) <- gets $ uslkup var
                     when (null $ parties ps)
                       $ error $ "Not implemented: Output " ++ pretty var ++ " at partyset Top!"
                     psMap <- gets aliases
                     traverse_ (\party -> tell $ Outputs $ singleton (dealias psMap party) (singleton (value var) val)) $ parties ps
    Oblivious var p2s body -> do let semanticBody (ObvBody fc0 fc1 fv) = do (_, selection) <- gets $ uslkup fv
                                                                            c0 <- semanticChoice fc0
                                                                            c1 <- semanticChoice fc1
                                                                            return $ (complement selection .&. c0) .|. (selection .&. c1)
                                                                            --sequence $ semanticChoice <$> bool fc0 fc1 selection
                                     semanticChoice :: f (ObvChoice f) -> Sem r w
                                     semanticChoice focf = case value focf of
                                       ObvLeaf vLeaf -> do (_, val) <- gets $ uslkup $ vLeaf <$ focf
                                                           return val
                                       ObvBranch bdy -> semanticBody bdy
                                 val <- semanticBody $ value body
                                 modify $ bind (value var) (value p2s, val)
                                 recordViews (value var) val (value p2s)
    Declaration fname pargs body -> do es@EvalState{funcContext} <- get
                                       put es{ funcContext = insert (value fname) (flattenPargs pargs, body) funcContext }
    Call fname pargs returns -> do evalST <- get
                                   let (reqArgs, prog) = fromMaybe (error $ "No such function " ++ pretty fname ++ ".")
                                                         . (!? value fname) . funcContext $ evalST
                                   argValues <- traverse (uncurry unpackPargs) pargs
                                   let argMap = zipWith coalescePargs reqArgs argValues
                                   put mempty{ varContext = fromList $ concatMap snd argMap
                                             , aliases = fromList $ fst <$> argMap }
                                   semantics prog
                                   possibleReturns <- gets varContext
                                   funcAliases <- gets aliases
                                   put evalST
                                   traverse_ ( \(newVar, funcVar) -> let errorCase = error $ pretty fname ++ " doesn't bind " ++ pretty funcVar ++ "."
                                                                         goodCase (ps, val) = modify . bind (value newVar)
                                                                                              $ (runIdentity $ dealiass funcAliases ps, val)
                                                                     in maybe errorCase goodCase $ possibleReturns !? value funcVar
                                             ) returns
  where recordViews :: Variable -> w -> PartySet -> Sem r ()
        recordViews var b (Parties ps) | null ps = error "Not implemented: Send to partyset Top!"
                                       | otherwise = do psMap <- gets aliases
                                                        traverse_ (
                                                            \party -> tell Views{viewsMap = singleton (dealias psMap party) $ singleton var [b]}
                                                          ) ps
        flattenPargs :: [(f Party, [f Variable])] -> [(Party, [Variable])]
        flattenPargs = (value <$$$>) . (first value <$>)
        unpackPargs :: f Party -> [f Variable] -> Sem r (Party, [w])
        unpackPargs party vars = sequence (value party, traverse (unpackParg party) vars)
        unpackParg :: f Party -> f Variable -> Sem r w
        unpackParg party var = do (ps, val) <- gets $ uslkup var
                                  unless(value party `isElementOf` ps)
                                    $ error $ "Variable " ++ pretty var ++ " cannot be used as an argument by " ++ pretty party
                                      ++ ", they don't have it."
                                  return val
        coalescePargs (funcParty, args) (realParty, vals) = ( (funcParty, Identity realParty),
                                                             zipWith (\var val -> (var, (Pty.singleton funcParty, val))) args vals )


algSemantics :: forall w f r.
                 (Members '[
                   Reader (Inputs w),
                   Input w,
                   Writer (Outputs w),
                   Writer (Views w),
                   State (EvalState f w)
                   ] r,
                  Proper f, Functor f, Pretty1 f,
                  Semanticable w) =>
                Algebra f -> Sem r (PartySet, w)
algSemantics (Literal b) = return (owners b, if bit $ value b then complement zeroBits else zeroBits)
algSemantics (Var var) = gets $ uslkup var
algSemantics (Xor a1 a2) = do (ps1, v1) <- algSemantics $ value a1
                              (ps2, v2) <- algSemantics $ value a2
                              return (fromJust $ ps1 `intersect` ps2, v1 `xor` v2)
algSemantics (And a1 a2) = do (ps1, v1) <- algSemantics $ value a1
                              (ps2, v2) <- algSemantics $ value a2
                              return (fromJust $ ps1 `intersect` ps2, v1 .&. v2)
algSemantics (Not alg) = do complement <$$> algSemantics (value alg)


deterministicEvaluation' :: forall w f.
                            (Proper f, Functor f, Traversable f, Pretty1 f,
                             Semanticable w) =>
                            Program f -> Inputs w -> Tapes w -> (EvalState f w, (Outputs w, Views w))
deterministicEvaluation' p is ts =
  let (vc, (views, (outputs, ()))) = run . runState (mempty @(EvalState f w))
                                   . runWriter @(Views w) . runWriter @(Outputs w)
                                   . runInputUnsafe ts . runReader is
                                   $ semantics p
  in (vc, (outputs, views))

verboseIOEvaluation :: forall w. (Semanticable w) => Program Located -> Inputs w -> Tapes w -> IO (EvalState Located w, (Outputs w, Views w))
verboseIOEvaluation p is ts =
  do (vc, (views, (outputs, ()))) <- runM . traceToStdout . runState (mempty @(EvalState Located w))
                                      . runWriter @(Views w) . runWriter @(Outputs w)
                                      . runInputUnsafe ts . runReader is
                                      $ verboseSemantics p
     return (vc, (outputs, views))

deterministicEvaluation :: (Proper f, Functor f, Traversable f, Pretty1 f,
                            Semanticable w) =>
                           Program f -> Inputs w -> Tapes w -> (Outputs w, Views w)
deterministicEvaluation p is ts =
  let (_, ret) = deterministicEvaluation' p is ts
  in ret
