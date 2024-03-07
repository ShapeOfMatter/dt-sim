module Circuit where

import Data.List (findIndex, intercalate, nub)
import Data.Stream (fromList, Stream(Cons))
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary, arbitrary, chooseAny, chooseInt, elements, Gen, genericShrink, getSize, resize, scale, sized, shrink, resize, vectorOf, oneof, infiniteListOf, listOf)

import Utils (Pretty, pretty)

type NodeName = String

infixr 5 :::

data Circuit where
  Reference :: NodeName -> Circuit
  Constant :: Bool -> Circuit
  (:&:) :: Circuit -> Circuit -> Circuit
  (:+:) :: Circuit -> Circuit -> Circuit
  deriving (Generic, Show)

prettyParens :: Circuit -> String
prettyParens (Reference name) = if null name then "⎵" else name
prettyParens (Constant val) = show $ fromEnum val
prettyParens c = "(" ++ pretty c ++ ")"

instance Pretty Circuit where
  --pretty (a :&: b) = prettyParens a ++ " ∧ " ++ prettyParens b
  --pretty (a :+: b) = prettyParens a ++ " ⊻ " ++ prettyParens b
  pretty (a :&: b) = prettyParens a ++ " * " ++ prettyParens b
  pretty (a :+: b) = prettyParens a ++ " + " ++ prettyParens b
  pretty r = prettyParens r

arbitraryCircuitWith :: [NodeName] -> Gen Circuit
arbitraryCircuitWith names = do size <- getSize
                                if 1 >= size
                                  then oneof $ (Constant <$> chooseAny) : [Reference <$> elements names | not $ null names]
                                  else do left <- chooseInt (1, size)
                                          a <- resize left $ arbitraryCircuitWith names
                                          b <- resize (1 `max` (size - left)) $ arbitraryCircuitWith names
                                          op <- elements [(:&:), (:+:)]
                                          return $ a `op` b

arbitraryNodeName :: Gen NodeName
arbitraryNodeName = ("v" ++) . show <$> sized (curry chooseInt 0)

instance Arbitrary Circuit where
  shrink = genericShrink
  arbitrary = sized (`vectorOf` arbitraryNodeName) >>= arbitraryCircuitWith


data Circuits where
  Nil :: Circuits
  (:::) :: Circuit -> Circuits -> Circuits
  Let :: [NodeName] -> Circuits -> Circuits -> Circuits
  deriving (Generic, Show)

instance Pretty Circuits where
  pretty Nil = "()"
  pretty (c ::: Nil) = prettyParens c
  pretty (c ::: cs) = "(" ++ pretty c ++ ", " ++ deparens (pretty cs) ++ ")"
    where deparens [] = []
          deparens char@[_] = char
          deparens str = let stripLeft = fromMaybe 0 $ findIndex (/= '(') str
                             stripRight = fromMaybe 0 $ findIndex (/= ')') (reverse str)
                             strip = stripLeft `min` stripRight
                         in drop strip $ take (length str - strip) str
  pretty (Let names values body) = "let " ++ intercalate ", " names ++ " = " ++ pretty values ++ " in " ++ pretty body

junkCircuitsAST :: Gen Circuits
junkCircuitsAST = do size <- getSize
                     sizes <- fromList <$> infiniteListOf (chooseInt (1, (size `div` 2) + 1))
                     junk sizes
  where junk :: Stream Int -> Gen Circuits
        junk sizes = do size <- getSize
                        if 1 >= size then return Nil
                                     else oneof [tuple sizes, letIn sizes]
        tuple (s `Cons` sizes) = do c <- scale (min s) (arbitrary @Circuit)
                                    cs <- scale (max 1 . flip (-) s) $ junk sizes
                                    return $ c ::: cs
        letIn (s `Cons` sizes) = do names <- listOf arbitraryNodeName
                                    values <- scale (min s) junkCircuitsAST
                                    body <- scale (max 1 . flip (-) s) $ junk sizes
                                    return $ Let names values body

arbitraryCleanAST :: [NodeName] -> [NodeName] -> Gen Circuits
arbitraryCleanAST sigma used = do size <- getSize
                                  sizes <- fromList <$> infiniteListOf (chooseInt (1, (size `div` 2) + 1))
                                  snd <$> build sigma used sizes
  where build :: [NodeName] -> [NodeName] -> Stream Int -> Gen ([NodeName], Circuits)
        build sigma' used' sizes = do size <- getSize
                                      if 1 >= size then return (used', Nil)
                                                   else oneof [tuple sigma' used' sizes, letIn sigma' used' sizes]
        tuple sigma' used' (s `Cons` sizes) = do c <- scale (min s) $ arbitraryCircuitWith sigma'
                                                 (u, cs) <- scale (max 1 . flip (-) s) $ build sigma' used' sizes
                                                 return (u, c ::: cs)
        letIn sigma' used' (s `Cons` sizes) = do usize <- min s <$> getSize
                                                 usizes <- fromList <$> infiniteListOf (chooseInt (1, (usize `div` 2) + 1))
                                                 (uv, values) <- scale (min s) $ build sigma' used' usizes
                                                 let names = ["v" ++ show n | n <- [length uv .. length uv + tupleLength values - 1]]
                                                 (ub, body) <- scale (max 1 . flip (-) s) $ build (names ++ sigma') (names ++ uv) sizes
                                                 return (ub, Let names values body)

instance Arbitrary Circuits where
  shrink = genericShrink
  arbitrary = oneof [junkCircuitsAST, arbitraryCleanAST [] []]


tupleLength :: Circuits -> Int
tupleLength Nil = 0
tupleLength (_ ::: cs) = 1 + tupleLength cs
tupleLength (Let _ _ body) = tupleLength body


freeVars :: Circuits -> [NodeName]
freeVars = nub . freeVars' []

freeVars' :: [NodeName] -> Circuits -> [NodeName]
freeVars' _ Nil = []
freeVars' gamma (c ::: cs) = freeVarsC gamma c <> freeVars' gamma cs
freeVars' gamma (Let names values body) = freeVars' gamma values <> freeVars' (names ++ gamma) body

freeVarsC :: [NodeName] -> Circuit -> [NodeName]
freeVarsC gamma (Reference name) | name `elem` gamma = []
                                 | otherwise = [name]
freeVarsC _ (Constant _) = []
freeVarsC gamma (a :&: b) = freeVarsC gamma a <> freeVarsC gamma b
freeVarsC gamma (a :+: b) = freeVarsC gamma a <> freeVarsC gamma b

validate :: [NodeName] -> Circuit -> Bool
validate = (null .) <$> freeVarsC

validations :: [NodeName] -> [NodeName] -> Circuits -> Bool
validations _ _ Nil = True
validations gamma used (c ::: cs) = validate gamma c && validations gamma used cs
validations gamma used (Let names values body) = let used' = names ++ used
                                                 in ("" /=) `all` names
                                                    && not ((('_' ==) . head) `any` names)
                                                    && not ((`elem` used) `any` names)
                                                    && length names == tupleLength values
                                                    && validations gamma used' values
                                                    && validations (names ++ gamma) used' body

basicEvaluation :: [(NodeName, Bool)] -> Circuit -> Maybe Bool
basicEvaluation sigma (Reference name) = name `lookup` sigma
basicEvaluation _ (Constant val) = Just val
basicEvaluation sigma (a :&: b) = (&&) <$> basicEvaluation sigma a <*> basicEvaluation sigma b
basicEvaluation sigma (a :+: b) = (/=) <$> basicEvaluation sigma a <*> basicEvaluation sigma b

basicEvaluations :: [(NodeName, Bool)] -> Circuits -> [Maybe Bool]
basicEvaluations = basicEvaluations' []

basicEvaluations' :: [NodeName] -> [(NodeName, Bool)] -> Circuits -> [Maybe Bool]
basicEvaluations' _ _ Nil = []
basicEvaluations' forbidden sigma (c ::: cs) = basicEvaluation sigma c : basicEvaluations' forbidden sigma cs
basicEvaluations' forbidden sigma (Let names values body) = let forbidden' = names ++ forbidden
                                                                valuations = basicEvaluations' forbidden' sigma values
                                                                maybeBindings = names `zip` valuations
                                                                bindings = sequenceA `mapMaybe` maybeBindings
                                                            in if length valuations == length names
                                                                  && isJust `all` valuations
                                                                  && not ((`elem` forbidden) `any` names)
                                                                 then basicEvaluations' forbidden' (bindings ++ sigma) body
                                                                 else [Nothing]



halfAdder :: Circuit -> Circuit -> Circuits
halfAdder a b = (a :+: b) ::: (a :&: b) ::: Nil

fullAdder :: String ->  Circuit -> Circuit -> Circuit -> Circuits
fullAdder unique a b carry = Let [mainSum, mainCarry] (halfAdder a b) (
    Let [carrySum, carryCarry] (halfAdder (Reference mainSum) carry)
    $ Reference carrySum ::: (Reference mainCarry :+: Reference carryCarry) ::: Nil
  )
  where mainSum = "mainSum" ++ unique; mainCarry = "mainCarry" ++ unique; carrySum = "carrySum" ++ unique; carryCarry = "carryCarry" ++ unique

adder :: [(NodeName, NodeName)] -> Circuits
adder = adder' Nothing
  where adder' Nothing [] = Nil
        adder' (Just c) [] = c ::: Nil
        adder' mc ((a, b):ins) = Let [lsb a b, carry a b]
                                     (bitAdder a b mc)
                                     $ Reference (lsb a b) ::: adder' (Just $ Reference $ carry a b) ins
        bitAdder a b Nothing = halfAdder (Reference a) (Reference b)
        bitAdder a b (Just c) = fullAdder ("_" ++ a ++ "_" ++ b) (Reference a) (Reference b) c
        lsb a b = "lstSigBit_" ++ a ++ "_" ++ b
        carry a b = "carry_" ++ a ++ "_" ++ b

