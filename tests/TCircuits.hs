module TCircuits where

import Data.Functor.Identity (Identity(..))
import Data.Bifunctor (first)
import Data.Either (isRight, fromRight)
import Data.Map (fromList)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing, fromJust)
import Distribution.TestSuite.QuickCheck
import Test.QuickCheck ((.&&.),
                        Arbitrary, arbitrary,
                        conjoin,
                        counterexample,
                        elements,
                        expectFailure,
                        Gen,
                        infiniteList,
                        listOf1,
                        NonNegative (getNonNegative),
                        noShrinking,
                        Property,
                        scale,
                        sublistOf,
                        Testable, whenFail)

import Circuit
import Choreography (corrupt, deterministicEvaluation, fakePos, Inputs(..), honest, Outputs(..), Variable(..))
import qualified Choreography as Cho
import GMW (gmw)
import Utils (pretty, Pretty)

tests :: IO [Test]
tests = do return [soundAndComplete
                  ,correctAdder
                  ,junkIsMostlyJunk
                  ,validIsAllValid
                  ,gmwImplementsCorrectly
                  ]


soundAndComplete :: Test
soundAndComplete = testProperty "All and only valid circuits evaluate." $ noShrinking \circuit ->
  let outputs = basicEvaluations [] circuit
      validity = validations [] [] circuit
  in counterexample (show (pretty circuit, validity, outputs)) if validity then isJust `all` outputs else isNothing `any` outputs

correctAdder :: Test
correctAdder = testProperty "The adder circuits actually work." $ \xs ys ->
  let width = length xs `max` length ys
      x = sum [fromEnum b * (2 ^ i) | (b, i :: Int) <- xs `zip` [0..]]
      y = sum [fromEnum b * (2 ^ i) | (b, i :: Int) <- ys `zip` [0..]]
      ans = x + y
      xNames = ["x" ++ show i | i <- [0 .. width - 1]]
      yNames = ["y" ++ show i | i <- [0 .. width - 1]]
      circuit = adder $ xNames `zip` yNames
      arguments = (xNames `zip` (xs ++ repeat False)) ++ (yNames `zip` (ys ++ repeat False))
      results = basicEvaluations arguments circuit
  in conjoin [ counterexample (show (xs, ys, pretty circuit)) $ validations (xNames ++ yNames) (xNames ++ yNames) circuit
             , counterexample (show (xs, ys, pretty circuit, results)) $ isJust `all` results
             , counterexample (show (xs, ys, pretty circuit, results, ans)) $
                 ans == sum [fromEnum b * (2 ^ i) | (Just b, i :: Int) <- results `zip` [0..]]
             ]

junkIsMostlyJunk :: Test
junkIsMostlyJunk = testProperty "The random generator that yields random Circuits yields mostly invalid jibberish." $ noShrinking $
  expectFailure $ validations [] [] <$> junkCircuitsAST

validIsAllValid :: Test
validIsAllValid = testProperty "The random generator that yields valid Circuits yields only valid circuits." $ noShrinking
  do c <- arbitraryCleanAST [] []
     return $ counterexample (pretty c) $ validations [] [] c

arbitraryNamespace :: (Arbitrary a) => Gen [(String, a)]
arbitraryNamespace = do len <- getNonNegative <$> arbitrary @(NonNegative Int)
                        names <- sequence [ (++ show i) <$> scale (`div` 10) (listOf1 $ elements ['a' .. 'u'])  --exclude _ and 'v' for simplicity.
                                            | i <- [1 .. len] ]
                        vals <- infiniteList
                        return $ names `zip` vals

gmwImplementsCorrectly :: Test
gmwImplementsCorrectly = testProperty "The implementation of GMW yields protocols that output the same thing as plain circuit evaluation." $ noShrinking
  do args <- arbitraryNamespace @Bool
     corruptSecrets <- sublistOf args
     let (argVars, _) = unzip args
     circuit <- arbitraryCleanAST argVars argVars
     tapes <- infiniteList
     return $ whenFail (print args) $
       succeedWith (validations argVars argVars) (basicEvaluations args) circuit (\answer' ->
         succeedWith (all isJust) (fromJust <$>) answer' (\answer ->
           let (cho'', outVars) = gmw (corrupt, honest) circuit $ fst <$> corruptSecrets
           in whenFail (putStrLn $ pretty cho'') $
             let (_, cho') =  fakePos 0 cho''
             in succeedWith isRight (fromRight undefined) (Cho.validate mempty cho') (\cho ->
               let inputs = Inputs $ fromList $ first Variable <$> args
                   (observed, _) = deterministicEvaluation cho inputs tapes
               in whenFail (print observed) $
                 let packedAnswer = Outputs $ Map.filter (not . null) $ fromList $ (, fromList $ outVars `zip` answer) . Identity <$> [corrupt, honest]
                 in whenFail (print packedAnswer) $
                   observed == packedAnswer
                   )))
  where succeedWith :: (Pretty a, Testable c) => (a -> Bool) -> (a -> b) -> a -> (b -> c) -> Property
        succeedWith p f a c = counterexample (pretty a) $ p a .&&. c (f a)

