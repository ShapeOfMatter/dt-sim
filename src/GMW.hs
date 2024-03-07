{-# LANGUAGE QuasiQuotes #-}
module GMW where

import Data.Char (toLower)
import Data.Foldable (forM_)
import Data.Functor.Identity (Identity (Identity))
import Data.List (uncons)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromMaybe)
import Polysemy (Members, run, Sem)
import Polysemy.Reader (ask, Reader, runReader)
import Polysemy.Writer (tell, Writer, runWriter)
import Text.Parsec (parse)

import Choreography (Party(..), programParser, removeContext)
import Choreography.AbstractSyntaxTree
import Circuit
import Utils (litFile, swapsIf, swapsUnless)

iii :: a -> Identity a
iii = Identity

header :: Program Identity
-- TODO: use template-haskell lift?
header = case removeContext <$> parse programParser "_compileTime_:snippits/GMW_header.cho" [litFile|snippits/GMW_header.cho|]
         of Right h -> h
            Left e -> error $ show e

sharesOf :: forall r.
            (Members '[Reader (Party, Party)
                      ] r) =>
            NodeName -> Sem r (Variable, Variable)
sharesOf n = do (Party p1, Party p2) <- ask
                return (mk p1, mk p2)
  where mk p = Variable . (toLower <$>) $ "_" ++ n ++ "_" ++ p

gmw :: (Party, Party) -> Circuits -> [NodeName] -> (Program Identity, [Variable])
gmw p12@(p1, p2) circuits p1Secrets =
  run . runWriter
  . runReader p12
  $ do tell header
       forM_ (freeVars circuits) \secret ->
           do let dontSwap = secret `elem` p1Secrets  -- good god why is this typing so stuborn to communicate to ghc?
                  choose1 :: forall a. (a, a) -> a
                  choose1 = swapsUnless dontSwap
                  choose2 :: forall a. (a, a) -> a
                  choose2 = swapsIf dontSwap
              let owner = choose1 p12
              let secVar = Variable $ toLower <$> secret
              shares <- sharesOf secret
              tell [iii $ Secret (iii secVar) (iii owner),
                    iii $ Call (iii $ FuncName "secret_share")
                               [(iii owner, [iii secVar]), (iii $ choose2 p12, [])]
                               [(iii $ choose1 shares, iii $ Variable "s1"), (iii $ choose2 shares, iii $ Variable "s2")]]
       let (outWires, outVars) = unzip [ ("__outwire" ++ show i, Variable $ "__outvalue" ++ show i)
                                         | i <- [1 .. tupleLength circuits]]
       vars <- gmwMany outWires circuits
       forM_ (vars `zip` outVars) \((v1, v2), outvar) ->
           tell [iii $ Call (iii $ FuncName "reveal")
                            [(iii p1, [iii v1]), (iii p2, [iii v2])]
                            [(iii outvar, iii $ Variable "y")],
                 iii $ Output $ iii outvar]
       return outVars

gmwMany :: forall r.
           (Members '[Reader (Party, Party),
                      Writer (Program Identity)
                     ] r) =>
           [NodeName] -> Circuits -> Sem r [(Variable, Variable)]
gmwMany targets circuits = case circuits of
  Nil -> return []
  c ::: cs -> do let (target, targets') = fromMaybe ("__unknown", []) $ uncons targets
                 v <- gmwSingleton target c
                 vs <- gmwMany targets' cs
                 return $ v : vs
  Let names values body -> do _ <- gmwMany names values
                              gmwMany targets body
                              --return $ vs1 <> vs2

gmwSingleton :: forall r.
                (Members '[Reader (Party, Party),
                           Writer (Program Identity)
                           ] r) =>
                NodeName -> Circuit -> Sem r (Variable, Variable)
gmwSingleton target c = do
  (target1, target2) <- sharesOf target
  let returns v1 v2 = do tell [iii $ Compute (iii target1) (iii v1),
                               iii $ Compute (iii target2) (iii v2)]
                         return (target1, target2)
  case c of
    Reference name -> do (r1, r2) <- sharesOf name
                         returns (Var $ iii r1) (Var $ iii r2)
    Constant val -> returns (Literal $ iii $ Bit val) (Literal $ iii $ Bit False)
    c1 :&: c2 -> do (leftAtP1, leftAtP2) <- gmwSingleton ("__" ++ target ++ "_l") c1
                    (rightAtP1, rightAtP2) <- gmwSingleton ("__" ++ target ++ "_r") c2
                    (p1, p2) <- ask
                    tell [iii $ Call (iii $ FuncName "and_gmw")
                                     [(iii p1, [iii leftAtP1, iii rightAtP1]),
                                      (iii p2, [iii leftAtP2, iii rightAtP2])]
                                     [(iii target1, iii $ Variable "out1"), (iii target2, iii $ Variable "out2")]]
                    return (target1, target2)
    _ :+: _ -> do (a :| algs) <- unpackXor target c
                  let collapse f = foldl (Xor . iii) (f a) (iii . f <$> algs)
                  returns (collapse fst) (collapse snd)

unpackXor :: forall r.
             (Members '[Reader (Party, Party),
                        Writer (Program Identity)
                       ] r) =>
             NodeName -> Circuit -> Sem r (NonEmpty (Algebra Identity, Algebra Identity))
unpackXor target (c1 :+: c2) = do a1 <- unpackXor ("__" ++ target ++ "_l") c1
                                  a2 <- unpackXor ("__" ++ target ++ "_r") c2
                                  return $ a1 <> a2
unpackXor _ (Reference name) = do (v1, v2) <- sharesOf name
                                  return $ (Var $ iii v1, Var $ iii v2) :| []
unpackXor _ (Constant val) = return $ (Literal $ iii $ Bit val, Literal $ iii $ Bit False) :| []
unpackXor target c@(_ :&: _) = do (v1, v2) <- gmwSingleton target c
                                  return $ (Var $ iii v1, Var $ iii v2) :| []


