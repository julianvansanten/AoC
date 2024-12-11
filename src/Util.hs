module Util (Parser, AoCShow(..)) where


import Text.Parsec
import Data.Functor.Identity (Identity)


-- | Parser type alias.
type Parser = ParsecT String () Identity


-- | Class to convert data type to AoC compatible String.
class AoCShow a where
    aocShow :: a -> String
