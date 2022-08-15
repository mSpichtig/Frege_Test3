import Data.BigDecimal
import Data.BigFloating

easydiff :: Fractional a => (a -> a) -> a -> a -> a
easydiff f x h = (f (x + h) - f x) / h

halve :: Fractional a => a -> a
halve x = x / 2

repEAT :: (t -> t) -> t -> [t]
repEAT f a = a : (repEAT f (f a))

differentiate :: Fractional b => b -> (b -> b) -> b -> [b]
differentiate h0 f x = map (easydiff f x) (repEAT halve h0)

within :: (Ord a, Num a) => a -> [a] -> a
within eps (a:b:rest) = if abs (a-b) <= eps then b else (within eps (b:rest)) 

elimerror :: (Fractional a, Integral t) => t -> [a] -> [a]
elimerror n (a:b:rest) = (((b*2^n)-a)/(2^n - 1)):(elimerror n (b:rest)) 

order :: (RealFrac a, Integral b, Floating a) => [a] -> b
order (a:b:c:rest) = round (log2 ((a-c)/(b-c)-1))   -- ACHTUNG, round rundet z.B. 100.5 auf 100 ab!
--order (a:b:c:rest) = roundBD (log2 ((a-c)/(b-c)-1)) (halfUp 0)

log2 :: Floating a => a -> a
log2 x = log x / log 2  

improve :: (RealFrac a, Floating a) => [a] -> [a]
improve s = elimerror (order s) s

second :: [a] -> a
second (a:b:rest) = b

super :: (RealFrac b, Floating b) => [b] -> [b]
super s = map second (repEAT improve s)


main :: IO ()
main = do
        putStrLn $ "within 0.01 (differentiate 1 sqrt 1) ergibt: " ++ show (within 0.01 (differentiate 1 sqrt 1))
--        putStrLn $ "within 0.01 (improve (differentiate 1 sqrt 1)) ergibt: " ++ show (within 0.01 (improve (differentiate 1 sqrt 1)))
--        putStrLn $ "within 0.01 (improve (improve (improve (differentiate 1 sqrt 1)))) ergibt: " ++ show (within 0.01 (improve (improve (improve (differentiate 1 sqrt 1)))))
--        putStrLn $ "within 0.01 (super (differentiate 1 sqrt 1)) ergibt: " ++ show (within 0.01 (super (differentiate 1 sqrt 1)))
