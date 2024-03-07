module Tests (
  tests,
  tests'
) where

import Data.Either (isRight)
import Data.Functor.Identity (Identity(..))
import Data.List (nub)
import Data.Map.Strict ((!), fromList, singleton, empty)
import qualified Data.Set as Set
import Distribution.TestSuite.QuickCheck
import Polysemy (run)
import Polysemy.Random (runRandom)
import Test.QuickCheck ((===), Arbitrary (arbitrary), arbitraryBoundedIntegral, counterexample, Gen, ioProperty, Property, vectorOf)
import Text.Parsec (runParser, parse)
import Text.Parsec.String (parseFromFile)
import System.Random (mkStdGen)

import Choreography hiding (singleton)
import qualified Choreography.Party as P
import Utils


tests :: IO [Test]
tests = do return tests'

tests' :: [Test]
tests' = [ tautology
         , testEasyCompute
         , testEasySecret
         , testEasyFlip
         , testEasySend
         , testEasyObliv
         , testLines
         , testOutput
         , testSend
         , testObliv
         , example
         , pipeline
         -- , oneOfFourOT
         , gmwAndGates
         , miniFunc
         , gmwMacroGates
         , renderRoundTrip
         , genIsVaid
         ]


tautology :: Test
tautology = testProperty "Tautology" \i -> (===) @Int i i

testEasyCompute :: Test
testEasyCompute = testProperty "Easy Compute" $ case compute of
    Compute (Location os' _, Variable "a") (_, Literal (_, Bit True)) | os' == os && os == top -> True
    _ -> error (pretty compute)
  where compute :: Statement Located
        Right [(Location os _, compute)] = validate mempty program
        Right program = runParser programParser () "hardcoded example" "a = 1"

testEasySecret :: Test
testEasySecret = testProperty "Easy Secret" $ case sec of
    Secret (Location os' _, Variable "a") (_, p) | os == os' && os' == P.singleton p1 && p == p1 -> True
    _ -> error (pretty sec)
  where sec' = runParser programParser () "hardcoded example" "a = SECRET @P1"
        sec :: Statement Located
        Right [(Location os _, sec)] = case sec' of
          Right r -> validate mempty r
          Left e -> error $ show e

testEasyFlip :: Test
testEasyFlip = testProperty "Easy Flip" $ case sec of
    Flip (Location os' _, Variable "a") (_, p) | os == os' && os' == Parties (Set.singleton p1) && p == p1 -> True
    _ -> error (pretty sec)
  where sec' = runParser programParser () "hardcoded example" "a = FLIP @P1"
        sec :: Statement Located
        Right [(Location os _, sec)] = case sec' of
          Right r -> validate mempty r
          Left e -> error $ show e

testEasySend :: Test
testEasySend = testProperty "Easy Send" $ case sec of
    Send (Location o2' _, p2s) (Location o1' _, Variable "a")
      | [p1] == (nub . concat $ Set.toList . parties <$> [o2', p2s])
        && [p2] == (nub . concat $ Set.toList . parties <$> [o1'])
        && [p1, p2] == (nub . concat $ Set.toList . parties <$> [o2]) -> True
    _ -> error (show sec)
  where context = singleton (Variable "a") (Parties $ Set.singleton p2)
        sec' = runParser programParser () "hardcoded example" "SEND a TO P1"
        sec :: Statement Located
        Right [(Location o2 _, sec)] = case sec' of
          Right r -> validate context r
          Left e -> error $ show e

testEasyObliv :: Test
testEasyObliv = testProperty "Easy Obliv" $ case sec of
    Oblivious (Location bindOs _, boundVar)
              (Location forOs _, forPs)
              (Location outerOs _, ObvBody (Location from0 _, ObvLeaf var0)
                                           (Location from1 _, ObvBranch (ObvBody (Location from10 _, ObvLeaf var10)
                                                                                 (Location from11 _, ObvLeaf var11)
                                                                                 (Location toInner _, innerSelect)))
                                           (Location toOuter _, outerSelect))
      | [p2] == (nub . concat $ Set.toList . parties <$> [bindOs, forOs, forPs, o2])
        && null (nub . concat $ Set.toList . parties <$> [outerOs, from0, from1, from10, from11, toOuter, toInner])
        && [Variable "a"] == nub [var0, var10, var11, outerSelect, innerSelect]
        && Variable "b" == boundVar
        -> True
    _ -> error (show sec)
  where context = singleton (Variable "a") top
        sec' = runParser programParser () "hardcoded example" "b = OBLIVIOUSLY [a, [a,a]?a]?a FOR P2"
        sec :: Statement Located
        Right [(Location o2 _, sec)] = case sec' of
          Right r -> validate context r
          Left e -> error $ show e

testLines :: Test
testLines = testProperty "Test multi-line program" $ 3 === length program
  where program :: Program Located
        program = either error id $ validate mempty $ either (error . show) id program'
        program' = runParser programParser () "hardcode example" "\
        \a = 1\n\
        \b = 1\n\
        \c = 0"

testOutput :: Test
testOutput = testProperty "Test Output" $ (outputs, views) === deterministicEvaluation program inputs tapes
  where program :: Program Located
        Right (Right program) = validate empty <$> runParser programParser () "hardcode example" "\
        \a = SECRET @P2\n\
        \OUTPUT a"
        inputs = Inputs $ singleton (Variable "a") True
        tapes = []
        views = Views empty
        outputs = Outputs $ singleton (Identity p2) $ singleton (Variable "a") True

testSend :: Test
testSend = testProperty "Test Send" $ (outputs, views) === deterministicEvaluation program inputs tapes
  where program :: Program Located
        Right (Right program) = validate empty <$> runParser programParser () "hardcode example" "\
        \a = 1\n\
        \SEND a TO P2"
        inputs = Inputs empty
        tapes = []
        views = Views $ singleton (Identity p2) $ singleton (Variable "a") [True]
        outputs = Outputs empty

testObliv :: Test
testObliv = testProperty "Test Obliv" $ do -- the gen monad
  b <- arbitrary @Bool
  let program' = validate empty <$> runParser programParser () "hardcode example" ("\
        \a = " ++ pretty b ++ "\n\
        \yes = 1\n\
        \no = 0\n\
        \b = OBLIVIOUSLY [no,yes]?a FOR P2\n\
        \OUTPUT b")
  let program = either error id $ either (error . show) id program'
  let outputs = Outputs $ singleton (Identity p2) $ singleton (Variable "b") b
  let (observed, _) = deterministicEvaluation program (Inputs empty) []
  return $ outputs === observed

example :: Test
example = testProperty "An easy example" $ (outputs, views) === deterministicEvaluation program inputs tapes
  where program :: Program Located
        Right (Right program) = validate empty <$> runParser programParser () "hardcode example" "\
        \rand = FLIP @P1\n\
        \sec = SECRET @P2\n\
        \comp1 = !(sec + 1)\n\
        \SEND comp1 TO P1\n\
        \y = rand ^ comp1\n\
        \OUTPUT y"
        inputs = Inputs $ singleton (Variable "sec") True
        tapes = [True]
        views = Views $ fromList [(Identity p1, fromList[(Variable "comp1", [True]),
                                                          (Variable "rand", [True])] )]
        outputs = Outputs $ singleton (Identity p1) $ singleton (Variable "y") True

pipeline :: Test
pipeline = testProperty "3partyXOR.cho gives correct outputs" $ ioProperty do
  program' <- parseFromFile programParser "examples/3partyXOR.cho"
  let program = either error id $ either (error . show) id $ validate empty <$> program'
  return do  -- The Gen Monad!
    let (c, h1, h2) = (corrupt, Party "H1", Party "H2")
    secrets <- vectorOf 3 (arbitrary @Bool)
    let inputs = Inputs $ fromList $ [Variable "c_in"
                                     ,Variable "h1_in"
                                     ,Variable "h2_in"] `zip` secrets
    tapes <- vectorOf 6 (arbitrary @Bool)
    let y = foldl (/=) False secrets
    let outputs = Outputs $ fromList $ [Identity c, Identity h1, Identity h2] `zip` repeat (singleton (Variable "y") y)
    let (observed, _) = deterministicEvaluation program inputs tapes
    return $ observed === outputs

{- -- Currently UNused!
oneOfFourOT :: Test
oneOfFourOT = testProperty "1-of-4 Oblivious Transfer" $ ioProperty oneOfFourOTIO
oneOfFourOTIO :: IO (Gen Property)
oneOfFourOTIO = do
  program' <- parseFromFile programParser "examples/1of4OT.cho"
  let program = either error id $ either (error . show) id $ validate empty <$> program'
  return do  -- The Gen Monad!
    messages <- vectorOf 4 (arbitrary @Bool)
    selection <- chooseInt (0,3)
    let sAsBits = [odd selection, 2 <= selection]
    let inputs = Inputs $ fromList $ [Variable "option_00"
                                     ,Variable "option_01"
                                     ,Variable "option_10"
                                     ,Variable "option_11"
                                     ,Variable "choice_i1"
                                     ,Variable "choice_i2"] `zip` (messages <> sAsBits)
    tapes <- vectorOf 4 (arbitrary @Bool)
    let y = messages !! selection
    let outputs = Outputs $ fromList $ [Identity p2] `zip` repeat (singleton (Variable "final") y)
    let (vc, (observed, _)) = deterministicEvaluation' program inputs tapes
    return $ counterexample (pretty vc) $ observed == outputs
    -}

gmwAndGates :: Test
gmwAndGates = testProperty "Two party three-arg AND in GMW" $ ioProperty $ gmwAndGatesIO "examples/3party2andGMW.cho"
gmwMacroGates :: Test
gmwMacroGates = testProperty "Two party three-arg AND in GMW using macros" $ ioProperty $ gmwAndGatesIO "examples/GMW_macros.cho"
gmwAndGatesIO :: String -> IO (Gen Property)
gmwAndGatesIO fileName = do
  program' <- parseFromFile programParser fileName
  let program = either error id $ either (error . show) id $ validate empty <$> program'
  return do  -- The Gen Monad!
    secrets <- vectorOf 3 (arbitrary @Bool)
    let inputs = Inputs $ fromList $ [Variable "c_in"
                                     ,Variable "h1_in"
                                     ,Variable "h2_in"] `zip` secrets
    tapes <- vectorOf 5 (arbitrary @Bool)
    let y = and secrets
    let outputs = Outputs $ fromList $ [Identity p1, Identity p2] `zip` repeat (singleton (Variable "y") y)
    let (vc, (observed, _)) = deterministicEvaluation' program inputs tapes
    return $ counterexample (pretty vc) $ observed == outputs

miniFunc :: Test
miniFunc = testProperty "Mini functions example" $ ioProperty miniFuncIO
miniFuncIO :: IO (Gen Property)
miniFuncIO = do
  program' <- parseFromFile programParser "examples/miniFunc.cho"
  let program = either error id $ either (error . show) id $ validate empty <$> program'
  return do  -- The Gen Monad!
    secrets <- vectorOf 2 (arbitrary @Bool)
    let inputs = Inputs $ fromList $ [Variable "c_in"
                                     ,Variable "h_in"] `zip` secrets
    let tapes = mempty
    let (nCIn, nHIn) = (not $ inputsMap inputs ! Variable "c_in", not $ inputsMap inputs ! Variable "h_in")
    let y = [(Variable "c_out", nCIn)
            ,(Variable "h_out1", nHIn)
            ,(Variable "h_out2", nHIn)]
    let outputs = Outputs $ fromList [(Identity corrupt, fromList y), (Identity honest, fromList y)]
    let views = Views $ fromList [(Identity corrupt, fromList [(Variable "na", [nHIn, nHIn])]),
                                  (Identity honest, fromList [(Variable "na", [nCIn])])]
    let (vc, (observedO, observedV)) = deterministicEvaluation' program inputs tapes
    return $ counterexample (pretty vc ++ pretty observedO ++ pretty observedV ++ pretty outputs ++ pretty views) $ observedO == outputs && observedV == views

renderRoundTrip :: Test
renderRoundTrip = testProperty "Parsing a render gets you the same program back." $ do
    p <- generateProgram
    let str = pretty p
    let program1 = either error id $ validate mempty . snd $ fakePos 0 p
    let program2 = either error id $ either (error . show) id $ validate mempty <$> parse programParser "render" str
    q <- mkStdGen <$> arbitraryBoundedIntegral
    let vals1 = run . runRandom q $ makeData @Bool 64 program1 $ structureFields program1 $ P.singleton honest <> P.singleton corrupt
    let vals2 = run . runRandom q $ makeData @Bool 64 program2 $ structureFields program2 $ P.singleton honest <> P.singleton corrupt
    return $ counterexample (pretty p) $ vals1 == vals2
{-renderRoundTripIO :: String -> IO (Gen Property)
renderRoundTripIO fileName = do
  program' <- parseFromFile programParser fileName
  let program1 = either error id $ either (error . show) id $ validate empty <$> program'
  let rendered = render program1
  let program2 = either error id $ either (error . show) id $ validate empty <$> parse programParser "render" rendered
  return do  -- The Gen Monad!
    secrets <- vectorOf 3 (arbitrary @Bool)
    let inputs = Inputs $ fromList $ [Variable "c_in"
                                     ,Variable "h1_in"
                                     ,Variable "h2_in"] `zip` secrets
    tapes <- vectorOf 5 (arbitrary @Bool)
    let (evalSt1, (observed1, v1)) = deterministicEvaluation' program1 inputs tapes
    let (evalSt2, (observed2, v2)) = deterministicEvaluation' program2 inputs tapes
    return $ whenFail do{putStrLn $ unlines [rendered, pretty observed1, pretty v1, pretty evalSt1,
                                             render program2, pretty observed2, pretty v2, pretty evalSt2]}
               $ observed1 == observed2 && v1 == v2 && evalSt1 `basicallyEqual` evalSt2
               -}

genIsVaid :: Test
genIsVaid = testProperty "The protocol generator only yields valid protocols." $ do p <- generateProgram
                                                                                    let v = validate mempty . snd $ fakePos 0 p
                                                                                    return $ counterexample (pretty p) $ isRight v

