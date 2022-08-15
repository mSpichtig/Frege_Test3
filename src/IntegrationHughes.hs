
main :: IO ()
main = do
        putStrLn $ "within 0.01 (integrate sqrt 0 1) ergibt: " ++ show (within 0.01 (integrate sqrt 0 1))
        putStrLn $ "within 0.01 (improve (integrate sqrt 0 1))) ergibt: " ++ show (within 0.01 (improve (integrate sqrt 0 1)))
        putStrLn $ "within 0.01 (improve (improve (improve (integrate sqrt 0 1)))) ergibt: " ++ show (within 0.01 (improve (improve (improve (integrate sqrt 0 1)))))
        putStrLn $ "within 0.01 (super (integrate sqrt 0 1))) ergibt: " ++ show (within 0.01 (super (integrate sqrt 0 1)))
        putStrLn $ "within 0.01 (integrate2 sqrt 0 1) ergibt: " ++ show (within 0.01 (integrate2 sqrt 0 1))
        putStrLn $ "within 0.01 (improve (integrate2 sqrt 0 1))) ergibt: " ++ show (within 0.01 (improve (integrate2 sqrt 0 1)))
        putStrLn $ "within 0.01 (improve (improve (improve (integrate2 sqrt 0 1)))) ergibt: " ++ show (within 0.01 (improve (improve (improve (integrate2 sqrt 0 1)))))
        putStrLn $ "within 0.01 (super (integrate2 sqrt 0 1))) ergibt: " ++ show (within 0.01 (super (integrate2 sqrt 0 1)))




 
easyintegrate :: Fractional a => (a -> a) -> a -> a -> a
easyintegrate f a b = (f a + f b) *(b-a)/2

addpair :: Num a => (a, a) -> a
addpair (a,b) = a + b

zip2 :: [a] -> [b] -> [(a, b)]
zip2 (a:restA) (b:restB) = (a,b):(zip2 restA restB)

integrate :: Fractional t => (t -> t) -> t -> t -> [t]
integrate f a b = (easyintegrate f a b) : (map addpair (zip2 (integrate f a mid) (integrate f mid b)))
    where
      mid = (a + b)/2

integ :: Fractional t => (t -> t) -> t -> t -> t -> t -> [t]
integ f a b fa fb = ((fa + fb)*(b-a)/2):(map addpair (zip2 (integ f a m fa fm) (integ f m b fm fb)))
    where
      m = (a+b)/2
      fm = f m

integrate2 :: Fractional t => (t -> t) -> t -> t -> [t]
integrate2 f a b = integ f a b (f a) (f b)


repEAT :: (t -> t) -> t -> [t]
repEAT f a = a : (repEAT f (f a))

within :: (Ord a, Num a) => a -> [a] -> a
within eps (a:b:rest) = if abs (a-b) <= eps then b else (within eps (b:rest))

relative :: (Ord a, Fractional a) => a -> [a] -> a
relative eps (a:b:rest) = if abs (a/b-1) <= eps then b else (relative eps (b:rest))

elimerror :: (Fractional a, Integral t) => t -> [a] -> [a]
elimerror n (a:b:rest) = (((b*2^n)-a)/(2^n - 1)):(elimerror n (b:rest)) 

order :: (RealFrac a, Integral b, Floating a) => [a] -> b
order (a:b:c:rest) = round (log2 ((a-c)/(b-c)-1))   -- ACHTUNG, round rundet z.B. 100.5 auf 100 ab!

log2 :: Floating a => a -> a
log2 x = log x / log 2  

improve :: (RealFrac a, Floating a) => [a] -> [a]
improve s = elimerror (order s) s

second :: [a] -> a
second (a:b:rest) = b

super :: (RealFrac b, Floating b) => [b] -> [b]
super s = map second (repEAT improve s)

