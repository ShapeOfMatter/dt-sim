module Choreography.Validate
where

import Control.Arrow (ArrowChoice(left))
import Control.Monad (when)
import Data.Functor.Identity (Identity(..))
import Data.List (intercalate)
import Data.Map.Strict ((!?), insertWith, Map, insert, fromList, toList)
import Polysemy (Members, run, Sem, reinterpretH)
import Polysemy.Error (Error (Throw, Catch), runError, throw)
import Polysemy.State (get, gets, put, runState, State)

import Choreography.AbstractSyntaxTree
import Choreography.Functors (Located, Location(..), Sourced)
import Choreography.Parser ()
import Choreography.Party hiding (insert)
import Utils (Pretty, pretty)


validate :: Map Variable PartySet -> Program Sourced -> Either String (Program Located)
validate context = left collapseError . snd . validate' context

validate' :: Map Variable PartySet -> Program Sourced -> (Map Variable PartySet, Either (ValidationState, Sourced String) (Program Located))
validate' context = cullState . run . runState mempty{variables=context} . runError . addStateToError . traverse validateStatement
  where cullState (ValidationState{variables}, p) = (variables, p)

addStateToError :: forall s e a r.
                   (Members '[State s] r) =>
                   Sem (Error e ': r) a -> Sem (Error (s, e) ': r) a
addStateToError = reinterpretH (\case
    Throw e -> do st <- get
                  throw (st, e)
    Catch _ _ -> error "Look that's just not how we're using it, sorry."
  )

collapseError :: (ValidationState, Sourced String) -> String
collapseError (vs, ss) = unlines [pretty ss, pretty vs]

validateStatement :: forall r.
                     (Members '[Error (Sourced String), State ValidationState] r) =>
                     Sourced (Statement Sourced) -> Sem r (Located (Statement Located ))
validateStatement (sourcePos, stmnt) = case stmnt of
  Compute fvar@(_, v) falg -> do (ps, alg) <- validateAlg falg
                                 bindToParties v ps
                                 locateLine ps $ Compute (locate ps fvar) alg
  Secret fvar@(_, v) fp@(_, p) -> do let ps = singleton p
                                     bindToParties v ps
                                     locateLine ps $ Secret (locate ps fvar) (locate ps fp)
  Flip fvar@(_, v) fp@(_, p) -> do let ps = singleton p
                                   bindToParties v ps
                                   locateLine ps $ Flip (locate ps fvar) (locate ps fp)
  Send fps@(_, ps2) fvar@(_, v) -> do ps1 <- lookupVar fvar
                                      bindToParties v ps2
                                      locateLine (ps1 `union` ps2) $ Send (locate ps2 fps) (locate ps1 fvar)
  Oblivious fvar@(_, v) fps@(_, ps2) fob -> do (ps1, body) <- validateObliv ps2 fob
                                               bindToParties v ps2
                                               locateLine (ps1 `union` ps2) $ Oblivious (locate ps2 fvar) (locate ps2 fps) body
  Output fvar -> do ps <- lookupVar fvar
                    locateLine ps $ Output (locate ps fvar)
  Declaration ffName pargs body -> do let lfName@(loc, fn) = locate top ffName
                                      let locateParg (fp@(_, p), vs) = (locate (singleton p) fp, locate (singleton p) <$> vs)
                                      let pargs' = locateParg <$> pargs
                                      let asKVP ((_, p), vs) = [(v, singleton p) | (_, v) <- vs]
                                      (vars, body') <- sequence $ either (throw . (source loc,) . collapseError) return <$> validate' (fromList $ concat $ asKVP <$> pargs') body
                                      st@ValidationState{funcArgs, funcReturns} <- get
                                      put st{funcArgs = insert fn [(p, length vs) | ((_, p), vs) <- pargs'] funcArgs,
                                             funcReturns = insert fn vars funcReturns}
                                      locateLine top $ Declaration lfName pargs' body'
  Call ffName pargs returns -> do let lfName@(_, fn) = locate top ffName
                                  (requiredArgs, possibleReturns) <- lookupFunc ffName
                                  let locateArg p fv@(src, var) = do ps <- lookupVar fv
                                                                     if p `isElementOf` ps
                                                                       then return $ locate (singleton p) fv
                                                                       else throw (src, pretty p ++ " can't use " ++ pretty var ++ ".")
                                  let locateParg (fp@(_, p), vs) = do vs' <- traverse (locateArg p) vs
                                                                      return (locate (singleton p) fp, vs')
                                  pargs' <- traverse locateParg pargs
                                  when ( (length . snd <$> pargs') /= (snd <$> requiredArgs) )
                                    $ throw (sourcePos, pretty fn ++ " has signature " ++ show requiredArgs ++ "; argument miss-match.")
                                  let aliases = fromList $ [(pfunc, Identity preal) | ((pfunc, _), ((_, preal), _)) <- zip requiredArgs pargs']
                                  let validateReturn (nb@(_, newBinding), ff@(srcff, fromFunc)) =
                                        do fowners <- maybe (throw (srcff, pretty fn ++ " doesn't afford variable " ++ pretty fromFunc ++ "."))
                                                            return
                                                            $ possibleReturns !? fromFunc
                                           let nowners = runIdentity $ dealiass aliases fowners
                                           bindToParties newBinding nowners
                                           return (locate nowners nb, locate fowners ff)
                                  returns' <- traverse validateReturn returns
                                  locateLine top $ Call lfName pargs' returns'
  where locateLine :: PartySet -> Statement Located -> Sem r (Located (Statement Located))
        locateLine ps st = return $ locate ps (sourcePos, st)

locate :: PartySet -> Sourced a -> Located a
locate ps (source, a) = (Location{source, lowners=ps}, a)

data ValidationState = ValidationState{ variables :: Map Variable PartySet
                                      , funcArgs :: Map FuncName [(Party, Int)]
                                      , funcReturns :: Map FuncName (Map Variable PartySet) }
                       deriving (Show)
instance Semigroup ValidationState where
  ValidationState v1 a1 r1 <> ValidationState v2 a2 r2 = ValidationState (v1 <> v2) (a1 <> a2) (r1 <> r2)
instance Monoid ValidationState where
  mempty = ValidationState mempty mempty mempty
instance Pretty ValidationState where
  pretty ValidationState{variables, funcArgs, funcReturns} = unlines
    $ ( (\(var, ps) -> "  " ++ pretty var ++ " @ " ++ pretty ps)  <$> toList variables )
    <> ( (\(fName, pargs) -> "  " ++ pretty fName ++ "(" ++ intercalate ", " [pretty p ++ " takes " ++ show n | (p, n) <- pargs] ++ ")") <$> toList funcArgs )
    <> ( (\(fName, namespace) -> "  " ++ pretty fName ++ " -> (" ++ intercalate ", " [pretty v ++ "@" ++ pretty ps | (v, ps) <- toList namespace] ++ ")") <$> toList funcReturns )

bindToParties :: forall r.
                 (Members '[State ValidationState] r) =>
                 Variable -> PartySet -> Sem r ()
bindToParties v ps = do vst@ValidationState{variables=vars} <- get
                        put vst{variables = insertWith (<>) v ps vars}

lookupVar :: forall r.
             (Members '[State ValidationState, Error (Sourced String)] r) =>
             Sourced Variable -> Sem r PartySet
lookupVar (source, var) = do mps <- gets $ (!? var) . variables
                             maybe (throw (source, "Variable " ++ pretty var ++ " is not in scope.")) return mps

lookupFunc :: forall r.
              (Members '[State ValidationState, Error (Sourced String)] r) =>
              Sourced FuncName -> Sem r ([(Party, Int)], Map Variable PartySet)
lookupFunc (source, fName) = do requiredArgs' <- gets $ (!? fName) . funcArgs
                                requiredArgs <- maybe (throw (source, "Function " ++ pretty fName ++ " is not in scope.")) return requiredArgs'
                                possibleReturns' <- gets $ (!? fName) . funcReturns
                                possibleReturns <- maybe (throw (source, "This should be impossible.")) return possibleReturns'
                                return (requiredArgs, possibleReturns)

validateObliv :: forall r.
                 (Members '[Error (Sourced String), State ValidationState] r) =>
                 PartySet -> Sourced (ObvBody Sourced) -> Sem r (PartySet, Located (ObvBody Located))
validateObliv ps2 (source, ObvBody fBranch tBranch choice)
  = do psc <- lookupVar choice
       (psf, fBranch') <- validateBranch fBranch
       (pst, tBranch') <- validateBranch tBranch
       let mps1 = psf `intersect` pst
       ps1 <- maybe (throw (source, "Nobody can obliviosly serve both branches.")) return mps1
       let participants = ps2 `union` ps1
       if ps2 `isSubsetOf` psc
         then return (ps1, locate participants (source, ObvBody fBranch' tBranch' (locate participants choice)))
         else throw (source, "Not all of " ++ pretty ps2 ++ " can obliviously choose by " ++ pretty choice ++ ".")
  where validateBranch :: Sourced (ObvChoice Sourced) -> Sem r (PartySet, Located (ObvChoice Located))
        validateBranch (src, ObvLeaf var) = do ps <- lookupVar (src, var)
                                               return (ps, locate ps (src, ObvLeaf var))
        validateBranch (src, ObvBranch body) = do (ps, (location, body')) <- validateObliv ps2 (src, body)
                                                  return (ps, (location{source=src}, ObvBranch body'))

validateAlg :: forall r.
               (Members '[Error (Sourced String), State ValidationState] r) =>
               Sourced (Algebra Sourced) -> Sem r (PartySet, Located (Algebra Located))
validateAlg (src, algebra) = case algebra of
  Literal fbit -> return (top, locate top (src, Literal $ locate top fbit))
  Var fvar -> do ps <- lookupVar fvar
                 return (ps, (Location{source=src, lowners=ps}, Var $ locate ps fvar))
  Xor fa1 fa2 -> do (ps1, alg1) <- validateAlg fa1
                    (ps2, alg2) <- validateAlg fa2
                    ps <- ps1 `inter` ps2
                    return (ps, (Location{source=src, lowners=ps}, Xor alg1 alg2))
  And fa1 fa2 -> do (ps1, alg1) <- validateAlg fa1
                    (ps2, alg2) <- validateAlg fa2
                    ps <- ps1 `inter` ps2
                    return (ps, (Location{source=src, lowners=ps}, And alg1 alg2))
  Not falg -> do (ps, alg@(location, _)) <- validateAlg falg
                 return (ps, (location{source=src}, Not alg))
  where ps1 `inter` ps2 = maybe (throw (src, "Nobody has all the pieces to compute " ++ pretty algebra ++ ".")) return $ ps1 `intersect` ps2

