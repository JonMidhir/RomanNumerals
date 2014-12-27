import qualified Data.Map.Strict as Map
import Data.Maybe

values = Map.fromList([(1000, "M"), (900, "CM"), (500, "D"), 
                        (400, "CD"), (100, "C"), (90, "XC"), 
                        (50, "L"), (40, "XL"), (10, "X"), 
                        (9, "IX"), (5, "V"), (4, "IV"), (1, "I")])

toRoman :: Int -> [Char]
toRoman 0 = []
toRoman a
  | exactValue /= "" = exactValue
  | otherwise = highestVal ++ toRoman(remainder)
  where exactValue  = Map.findWithDefault "" a values
        remainder   = a - (fst nextHighest)
        highestVal  = snd nextHighest
        nextHighest = fromMaybe (0, "") $ Map.lookupLT a values