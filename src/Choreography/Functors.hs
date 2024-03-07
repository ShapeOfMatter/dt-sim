module Choreography.Functors where

import Control.DeepSeq (NFData, rnf)
import Data.Bifunctor (first)
import Data.Functor.Identity (Identity (Identity), runIdentity)
import GHC.Generics (Generic)
import Text.Parsec (SourcePos)
import qualified Text.Parsec.Pos as Pos

import Choreography.AbstractSyntaxTree (Algebra(..), ObvBody(..), ObvChoice(..), Statement(..), Program)
import Choreography.Party (PartySet)
import Utils ((<$$>), (<$$$>), Pretty, pretty)

type Sourced = (,) SourcePos

class Proper f where
  owners :: f a -> PartySet
  value :: f a -> a
instance Proper ((,) PartySet) where
  owners = fst
  value = snd

data Location = Location { lowners :: PartySet, source :: SourcePos } deriving (Eq, Generic, Ord, Show)
instance Pretty Location where pretty = show
instance NFData Location where rnf Location{lowners, source} = lowners `seq` source `seq` ()  -- it probably doesn't matter if this is perfect?
type Located = (,) Location
instance Proper Located where
  owners = lowners . fst
  value = snd

data Improper = Improper { iowners :: Maybe PartySet, isource :: SourcePos } deriving (Eq, Ord, Show)
type ILocated = (,) Improper

class AntiFunctor d where
  antimap :: (Functor f, Functor g) => (forall a. f a -> g a) -> d f -> d g

antimap' :: (AntiFunctor d, Functor f, Functor g) => (forall a. f a -> g a) -> f (d f) -> g (d g)
antimap' t d = antimap t <$> t d

instance AntiFunctor Algebra where
  antimap t a = case a of
    Literal f -> Literal $ t f
    Var f -> Var $ t f
    Xor f1 f2 -> antimap' t f1 `Xor` antimap' t f2
    And f1 f2 -> antimap' t f1 `And` antimap' t f2
    Not f -> Not $ antimap' t f

instance AntiFunctor ObvBody where
  antimap t (ObvBody f1 f2 fv) = ObvBody (antimap' t f1) (antimap' t f2) (t fv)

instance AntiFunctor ObvChoice where
  antimap _ (ObvLeaf v) = ObvLeaf v
  antimap t (ObvBranch ob) = ObvBranch $ antimap t ob

instance AntiFunctor Statement where
  antimap t s = case s of
    Compute fv fa -> Compute (t fv) (antimap' t fa)
    Secret fv fp -> Secret (t fv) (t fp)
    Flip fv fp -> Flip (t fv) (t fp)
    Send fps fv -> Send (t fps) (t fv)
    Oblivious fv fps fob -> Oblivious (t fv) (t fps) (antimap' t fob)
    Output fv -> Output (t fv)
    Declaration fn args fp -> Declaration (t fn) (first t <$> t <$$$> args) (antimap' t <$> fp)
    Call fn args gets -> Call (t fn) (first t <$> t <$$$> args) (first t <$> t <$$> gets)


changeFunctor :: (Functor f, Functor g) => (forall a. f a -> g a) -> Program f -> Program g
changeFunctor t p = antimap' t <$> p

removeContext :: Program ((,) a) -> Program Identity
removeContext = changeFunctor $ Identity . snd


fakePos :: Pos.Line -> Program Identity -> (Pos.Line, Program Sourced)
fakePos l [] = (l, [])
fakePos l (Identity stmnt : ps) = case stmnt of
  Declaration fName pargs body -> let (l', body') = fakePos (l + 1) body
                                      decl = (pos, Declaration (atPos fName) ((atPos <$$>) . first atPos <$> pargs) body')
                                  in (decl :) <$> fakePos l' ps
  _ -> ((pos, antimap atPos stmnt) :) <$> fakePos (l + 1) ps
  where pos = Pos.newPos "__AST" l (-1)
        atPos :: Identity a -> Sourced a
        atPos = (pos,) . runIdentity


