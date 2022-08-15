import Data.BigDecimal
import Data.BigFloating

-- A case generating a result:
-- toString $ head $ take 1 $ (super (differentiate (BigDecimal 1 0) sqrt (BigDecimal 1 0)))


easydiff :: Fractional a => (a -> a) -> a -> a -> a
easydiff f x h = (f (x + h) - f x) / h

halve :: Fractional a => a -> a
halve x = x / 2

repEAT :: (t -> t) -> t -> [t]
repEAT f a = a : (repEAT f (f a))

differentiate :: Fractional b => b -> (b -> b) -> b -> [b]
--differentiate :: BigDecimal -> (BigDecimal -> BigDecimal) -> BigDecimal -> [BigDecimal]
differentiate h0 f x = map (easydiff f x) (repEAT halve h0)

within :: (Ord a, Num a) => a -> [a] -> a
within eps (a:b:rest) = if abs (a-b) <= eps then b else (within eps (b:rest)) 


elimerror :: Integral t => t -> [BigDecimal] -> [BigDecimal]
elimerror n (a:b:rest) = (((b*2^n)-a)/(2^n - 1)):(elimerror n (b:rest)) 

-- MAIN DIFFERENCE IS HERE
--order :: (RealFrac a, Integral b, Floating a) => [a] -> b
order :: Integral t => [BigDecimal] -> t
order (a:b:c:rest) = round (log2 $ dbl $ ((a-c)/(b-c)-1))   -- ACHTUNG, round rundet z.B. 100.5 auf 100 ab!
  where 
   dbl b = (fromIntegral$getValue b)/(10**(fromIntegral$getScale b) )

log2 :: Floating a => a -> a
log2 x = log x / log 2  

--improve :: (RealFrac a, Floating a) => [a] -> [a]
improve :: [BigDecimal] -> [BigDecimal]
improve s = elimerror (order s) s

second :: [a] -> a
second (a:b:rest) = b

--super :: (RealFrac b, Floating b) => [b] -> [b]
super :: [BigDecimal] -> [BigDecimal]
super s = map second (repEAT improve s)


