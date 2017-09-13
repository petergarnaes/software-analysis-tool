module Environment where
import qualified Prelude
import Prelude hiding (lookup)
import Lattice
import Data.Map
import Control.Applicative
import BasicTypes

newtype Environment a b = Environment { runEnvironment :: (Map Ident a) -> (b, (Map Ident a))}

instance (Lattice a) => Monad (Environment a) where
    return x = Environment (\map -> (x,map))
    env >>= f = Environment $ \map -> let (x,map2) = runEnvironment env map in
        runEnvironment (f x) map2

-- Functor and Applicative from slides like you copy pasted for us
instance (Lattice a) => Functor (Environment a) where
  fmap f xs = xs >>= return . f

instance (Lattice a) => Applicative (Environment a) where
  pure = return
  df <*> dx = df >>= \f -> dx >>= return . f

getLatticeElement :: (Lattice a) => Ident -> Environment a a
getLatticeElement id = Environment $ \map -> case lookup id map of
    Just val -> (val,map)
    Nothing -> (initialize,map)

insertElement :: (Lattice a) => Ident -> a -> Environment a ()
insertElement id lat = Environment $ \map -> ((),insert id lat map)

getMap :: (Lattice a) => Environment a b -> Environment a (Map Ident a)
getMap env = Environment $ \map -> let (_,map2) = runEnvironment env map in (map2,map2)

leastUpperBound :: (Lattice a) => Environment a b -> Environment a b -> Environment a ()
leastUpperBound env1 env2 = do
    map1 <- getMap env1
    map2 <- getMap env2
    Environment $ \map -> ((),unionWith bound map1 map2)
