module Utils
where

import Control.Exception (evaluate, Exception, try)
import Data.Bits (finiteBitSize, testBit)
import Data.Either (fromRight)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Kind (Type)
import Data.List (uncons)
import Data.Map.Strict (adjust, Map)
import Data.Maybe (fromJust, fromMaybe)
import Data.Word (Word64)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as THQuote
import Polysemy (interpretH, reinterpret, Sem)
import Polysemy.Error (Error(Catch, Throw))
import Polysemy.Fail (Fail, runFail)
import Polysemy.Input (Input(..))
import Polysemy.State (get, put, runState)
import Text.Parsec hiding (try, uncons)


class Pretty a where
  pretty :: a -> String
instance {-# OVERLAPPING  #-} Pretty String where
  pretty = id
instance {-# OVERLAPPABLE #-} (Show n, Num n) => Pretty n where
  pretty = show
instance Pretty Bool where
  pretty b = show $ fromEnum b
{-instance Pretty Rational where
  pretty r = case (numerator r, denominator r) of
               (n, 1) -> show n
               (n, d) -> show n <> "/" <> show d-}
instance Pretty SourcePos where
  pretty sp = "(L" ++ show (sourceLine sp) ++ ";C" ++ show (sourceColumn sp) ++ ")"
instance Pretty Word64 where
  pretty w = concat [pretty $ testBit w i | i <- [0 .. finiteBitSize w - 1] ]

prettyPrint :: (Pretty a) => a -> IO ()
prettyPrint = putStrLn . pretty

class Pretty1 (f :: Type -> Type) where
  prettyf :: f String -> String
instance Pretty1 Identity where
  prettyf = runIdentity
instance (Pretty meta) => Pretty1 ((,) meta) where
  prettyf (k, s) = pretty k <> ": " <> s
instance Pretty1 [] where
  prettyf = unlines
instance Pretty1 Maybe where
  prettyf = fromMaybe "⎵"
instance Pretty1 (Either String) where
 prettyf = either ("Left" ++) ("Right" ++)
instance {-# OVERLAPPABLE #-} (Pretty1 f, Functor f, Pretty a) => Pretty (f a) where
  pretty = prettyf . (pretty <$>)


runFailOr :: forall a r.
             a -> Sem (Fail ': r) a -> Sem r a
runFailOr a = (fromRight a <$>) . runFail

(<$$>) :: forall f g a b. (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
func <$$> struct = (func <$>) <$> struct

(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
func <$$$> struct = (func <$$>) <$> struct

expand :: (Ord k, Semigroup a) => k -> a -> Map k a -> Map k a
expand k a = adjust (<> a) k

-- https://stackoverflow.com/a/17970063/10135377
changeState
  :: forall m s u v a . (Functor m, Monad m)
  => (u -> v)
  -> (v -> u)
  -> ParsecT s u m a
  -> ParsecT s v m a
changeState forward backward = mkPT . transform . runParsecT
  where
    mapState :: forall u' v'.
                (u' -> v') -> State s u' -> State s v'
    mapState f st = st { stateUser = f (stateUser st) }

    mapReply :: forall u' v'.
                (u' -> v') -> Reply s u' a -> Reply s v' a
    mapReply f (Ok a st err) = Ok a (mapState f st) err
    mapReply _ (Error e) = Error e

    fmap3 = fmap . fmap . fmap

    transform
      :: (State s u -> m (Consumed (m (Reply s u a))))
      -> (State s v -> m (Consumed (m (Reply s v a))))
    transform p st = fmap3 (mapReply forward) (p (mapState backward st))


-- https://stackoverflow.com/a/35600656/10135377
throwsException :: forall e a. Exception e => IO a -> IO Bool
throwsException ioa = do eea <- try ioa
                         either (const @(IO Bool) @e (return True)) (
                             fmap (either (const @Bool @e True) (const False)) . try . evaluate
                           ) eea

-- https://stackoverflow.com/a/12717160/10135377
-- Takes one arg: a file name.
litFile :: THQuote.QuasiQuoter
litFile = THQuote.quoteFile $ THQuote.QuasiQuoter{ THQuote.quoteExp = return . TH.LitE . TH.StringL
                                                 , THQuote.quotePat=undefined
                                                 , THQuote.quoteType=undefined
                                                 , THQuote.quoteDec=undefined }

runInputUnsafe
    :: [i]
    -> Sem (Input i ': r) a
    -> Sem r a
runInputUnsafe is = fmap snd . runState is . reinterpret
  (\case
      Input -> do
        sss <- get
        let (s, ss) = fromJust $ uncons sss
        put ss
        pure s
  )



swapsUnless :: Bool -> forall a. (a, a) -> a
swapsUnless predicate = if predicate then fst else snd
swapsIf :: Bool -> forall a. (a, a) -> a
swapsIf predicate = if predicate then snd else fst



-- https://wiki.haskell.org/Generic_number_type#squareRoot
squareRoot :: (Integral a) => a -> a
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let (^!) :: Num a => a -> Int -> a
       (^!) x e = x^e
       twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
   in  head $ dropWhile (not . isRoot) iters



failOnError :: forall e a r.
               (Show e) =>
               Sem (Error e ': r) a -> Sem r a
failOnError = interpretH (\case
    Throw e -> error $ show e
    Catch _ _ -> error "LOL, pretty sure it's too late for that!"
  )
