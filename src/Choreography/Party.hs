module Choreography.Party
where

import Control.DeepSeq (NFData)
import Data.Functor.Identity (Identity(..))
import Data.List (intercalate)
import Data.Map.Strict ((!?), Map)
import Data.Maybe (fromMaybe)
import Data.Set (disjoint, empty, Set, toList)
import qualified Data.Set as Set
import GHC.Generics (Generic)

import Utils (Pretty, pretty)

newtype Party = Party {party :: String}
           deriving (Eq, Generic, Ord, Read, Show)

instance NFData Party where {}

instance Pretty Party where
  pretty = party

newtype PartySet = Parties {parties :: Set Party} deriving (Eq, Generic, Ord, Show)

instance NFData PartySet where {}

instance Pretty PartySet where
  pretty (Parties ps) | null ps = "⊤"
                      | otherwise = intercalate ", " . toList $ pretty `Set.map` ps

instance Semigroup PartySet where
 (<>) = union

top :: PartySet
top = Parties empty

singleton :: Party -> PartySet
singleton = Parties . Set.singleton

union :: PartySet -> PartySet -> PartySet
(Parties ps1) `union` (Parties ps2) | null ps1 = Parties empty
                                    | null ps2 = Parties empty
                                    | otherwise = Parties $ ps1 `Set.union` ps2

insert :: Party -> PartySet -> PartySet
p `insert` (Parties ps) | null ps = Parties empty
                        | otherwise = Parties $ p `Set.insert` ps

intersect :: PartySet -> PartySet -> Maybe PartySet
(Parties ps1) `intersect` (Parties ps2) | null ps1 = Just $ Parties ps2
                                        | null ps2 = Just $ Parties ps1
                                        | disjoint ps1 ps2 = Nothing
                                        | otherwise = Just $ Parties $ ps1 `Set.intersection` ps2

isSubsetOf :: PartySet -> PartySet -> Bool
(Parties ps1) `isSubsetOf` (Parties ps2) | null ps2 = True
                                         | null ps1 = False  -- unless masked by null ps2!
                                         | otherwise = ps1 `Set.isSubsetOf` ps2

isElementOf :: Party -> PartySet -> Bool
p `isElementOf` (Parties ps) | null ps = True
                             | otherwise = p `Set.member` ps

type Concrete = Identity

dealiass :: Map Party (Concrete Party) -> PartySet -> Concrete PartySet
dealiass m (Parties ps) = Identity $ Parties $ (runIdentity . dealias m) `Set.map` ps

dealias :: Map Party (Concrete Party) -> Party -> Concrete Party
dealias m p = fromMaybe (Identity p) $ m !? p

p1 :: Party
p1 = Party "P1"  -- "ℙ𝟙"
p2 :: Party
p2 = Party "P2"  -- "ℙ𝟚"
p3 :: Party
p3 = Party "P3"  -- "ℙ𝟛"
honest :: Party
honest = Party "H"
corrupt :: Party
corrupt = Party "C"
