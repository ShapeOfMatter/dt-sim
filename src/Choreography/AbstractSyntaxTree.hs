module Choreography.AbstractSyntaxTree where

import Control.DeepSeq (NFData, NFData1)
import Data.List (intercalate)
import GHC.Generics (Generic)

import Choreography.Party (Party(..), PartySet)
import Utils (Pretty, pretty, Pretty1)


newtype Variable = Variable {variable :: String} deriving (Eq, Generic, NFData, Ord, Show)
instance Pretty Variable where pretty = variable

newtype FuncName = FuncName {funcName :: String} deriving (Eq, Generic, NFData, Ord, Show)
instance Pretty FuncName where pretty = funcName

newtype Bit = Bit { bit :: Bool } deriving (Bounded, Enum, Eq, Generic, NFData, Ord, Show)
trueNames :: [String]
trueNames = ["1", "true"]
falseNames :: [String]
falseNames = ["0", "false"]
instance Pretty Bit where
  pretty (Bit True) = head trueNames
  pretty (Bit False) = head falseNames

data Algebra f = Literal (f Bit)
               | Var (f Variable)
               | Xor (f (Algebra f)) (f (Algebra f))
               | And (f (Algebra f)) (f (Algebra f))
               | Not (f (Algebra f)) deriving (Generic)
deriving instance (forall a. (Show a) => Show (f a)) => Show (Algebra f)
instance (NFData1 f, forall b. (NFData b) => NFData (f b)) => NFData (Algebra f) where {}
xorNames :: [String]
xorNames = ["⊕", "XOR", "+", "<>", "!=", "⊻"]
andNames :: [String]
andNames = ["∧", "AND", "*", "^"]
notNames :: [String]
notNames = ["¬", "NOT", "!", "~"]
instance (Pretty1 f, Functor f) => Pretty (Algebra f) where
  pretty (Literal fb) = pretty fb
  pretty (Var fv) = pretty fv
  pretty (Xor fa1 fa2) = "(" ++ unwords [pretty fa1, head xorNames, pretty fa2] ++ ")"
  pretty (And fa1 fa2) = "(" ++ unwords [pretty fa1, head andNames, pretty fa2] ++ ")"
  pretty (Not fa) = "(" ++ unwords [head notNames, pretty fa] ++ ")"

data ObvBody f = ObvBody (f (ObvChoice f)) (f (ObvChoice f)) (f Variable) deriving (Generic)
deriving instance (forall a. (Show a) => Show (f a)) => Show (ObvBody f)
instance (NFData1 f, forall b. (NFData b) => NFData (f b)) => NFData (ObvBody f) where {}
data ObvChoice f = ObvLeaf Variable
                 | ObvBranch (ObvBody f)
                 deriving (Generic)
deriving instance (forall a. (Show a) => Show (f a)) => Show (ObvChoice f)
instance (NFData1 f, forall b. (NFData b) => NFData (f b)) => NFData (ObvChoice f) where {}
oblivSymbols :: [String]
oblivSymbols = ["[ ", ", ", " ]"]
choiceKeyword :: String
choiceKeyword = "?"
instance (Pretty1 f, Functor f) => Pretty (ObvBody f) where
  pretty (ObvBody fc0 fc1 fv) = head oblivSymbols ++ pretty fc0 ++ oblivSymbols !! 1 ++ pretty fc1 ++ oblivSymbols !! 2
                                ++ choiceKeyword ++ pretty fv
instance (Pretty1 f, Functor f) => Pretty (ObvChoice f) where
  pretty (ObvLeaf v) = pretty v
  pretty (ObvBranch body) = pretty body

data Statement f = Compute (f Variable) (f (Algebra f))
                 | Secret (f Variable) (f Party)
                 | Flip (f Variable) (f Party)
                 | Send (f PartySet) (f Variable)
                 | Oblivious (f Variable) (f PartySet) (f (ObvBody f))
                 | Output (f Variable)
                 | Declaration (f FuncName) [(f Party, [f Variable])] (Program f)
                 | Call (f FuncName) [(f Party, [f Variable])] [(f Variable, f Variable)]
                 deriving (Generic)
deriving instance (forall a. (Show a) => Show (f a)) => Show (Statement f)
instance (NFData1 f, forall b. (NFData b) => NFData (f b)) => NFData (Statement f) where {}
bindKeyword :: String
bindKeyword = "="
atKeyword :: String
atKeyword = "@"
secretKeyword :: String
secretKeyword = "SECRET"
flipKeyword :: String
flipKeyword = "FLIP"
sendKeywords :: [String]
sendKeywords = ["SEND", "TO"]
oblivKeywords :: [String]
oblivKeywords = ["OBLIVIOUSLY", "FOR"]
outputKeyword :: String
outputKeyword = "OUTPUT"
macroKeywords :: [String]
macroKeywords = ["MACRO", "AS", "ENDMACRO"]
callKeywords :: [String]
callKeywords = ["DO", "GET"]
instance (Pretty1 f, Functor f) => Pretty (Statement f) where
  pretty (Compute fv fa) = unwords [pretty fv, bindKeyword, pretty fa]
  pretty (Secret fv fp) = unwords [pretty fv, bindKeyword, secretKeyword, atKeyword <> pretty fp]
  pretty (Flip fv fp) = unwords [pretty fv, bindKeyword, flipKeyword, atKeyword <> pretty fp]
  pretty (Send fps fv) = unwords [head sendKeywords,
                                  pretty fv,
                                  sendKeywords !! 1,
                                  pretty fps]
  pretty (Oblivious fv fps fb) = unwords [pretty fv,
                                      pretty bindKeyword,
                                      head oblivKeywords,
                                      pretty fb,
                                      oblivKeywords !! 1,
                                      pretty fps]
  pretty (Output fv) = unwords [outputKeyword, pretty fv]
  pretty (Declaration ffName pargs prog) = unlines [ unwords [head macroKeywords,
                                                              pretty ffName <> prettyArgsList pargs,
                                                              macroKeywords !! 1]
                                                   , unlines $ ("  " <>) <$> lines (pretty prog)
                                                   , macroKeywords !! 2 ]
  pretty (Call ffName inArgs outArgs) = unwords [head callKeywords, call, callKeywords !! 1, bindings]
                                          where call = pretty ffName <> prettyArgsList inArgs
                                                bindings = "(" <> intercalate ", " ((\(a,b) -> pretty a <> "=" <> pretty b) <$> outArgs) <> ")"

prettyArgsList :: (Pretty1 f, Functor f) => [(f Party, [f Variable])] -> String
prettyArgsList pargs = pAL (prettyPair <$> pargs)
  where pAL xs = "(" <> intercalate ", " xs <> ")"
        prettyPair (fp, fvs) = pretty fp <> pAL (pretty <$> fvs)

type Program f = [f (Statement f)]



gatherSelectionVars :: (Foldable f) => ObvBody f -> [Variable]
gatherSelectionVars (ObvBody fc0 fc1 fv) = concatMap (:[]) fv <> concatMap gBranch fc0 <> concatMap gBranch fc1
  where gBranch (ObvLeaf _) = []
        gBranch (ObvBranch body) = gatherSelectionVars body

