{-# LANGUAGE ParallelListComp #-}
-- ableitung' sqrt (1) [0.3,0.2,0.15,0.1]
ableitung' f x hValues = polyInterp' (zip hValues $ map (f0 x) hValues) 0
  where
     f0 x h = (f (x+h) - f x)/(h)

-- ableitung'' sin (pi/4) [0.3,0.2,0.15,0.1] ist Beispiel aus Buch S. 131
ableitung'' f x hValues = polyInterp' (zip hValues $ map (f0 x) hValues) 0
  where
     f0 x h = (f (x+h) - 2*f x + f (x-h))/(h*h)

simpson :: (Fractional a, Enum a) => (a -> a) -> a -> a -> a -> a
simpson f a b n = h*(f a + 4 * f (a + h) + f b + 2* sum)/3
  where
   h = (b - a) / (2*n)
   sum = foldl (\c k -> c + f (a+2*k*h) + 2 * f (a + (2*k+1) *h)) 0.0 [1 .. n-1]
   
gaussTschebyscheffQuadratur f a b n = integral  * pi * (b - a) / (2*n)
 where
  integral = foldl (\c i -> let x = cos ((2*i-1)*pi / (2*n)) in (c + f (x*(b-a)*0.5+(a+b)*0.5) * sqrt (1 - x*x) ) ) 0.0 [1 .. n]

polyInterp :: Fractional a => [(a,a)] -> a -> a
polyInterp xys = head . last . neville xys

polyInterp' :: Fractional a => [(a,a)] -> a -> a
polyInterp' xys = head . last . neville' xys

neville :: Fractional a => [(a,a)] -> a -> [[a]]
neville xys x = table
    where
        (xs,ys) = unzip xys
        table = ys :
            [ [ ((x - x_j) * p1 + (x_i - x) * p0) / (x_i - x_j) 
              | p0:p1:_ <- tails' row
              | x_j     <- xs
              | x_i     <- x_is
              ]
            | row  <- table
            | x_is <- tail (tails' xs)
 --           , not (null x_is)   --- this line is not necessary as tails' differs in this regard from Data.List.tails
            ]

neville' :: Fractional a => [(a,a)] -> a -> [[a]]
neville' xys x = ys : zipo zipxs zipys
   where 
     (xs,ys) = unzip xys
     zipxs = zip (init xs) (tail xs)
     zipys = zip (init ys) (tail ys)
     zipo [] _ = []
     zipo zx zy = bs : zipo zzx zzy 
      where
       bs = map (\((x1,x2),(p1,p2)) -> ((x-x2)*p1-(x-x1)*p2) / (x1 - x2) ) $ zip zx zy
       (fzx,szx) = unzip zx
       zzx = zip (init fzx) (tail szx) 
       zzy = zip (init bs) (tail bs)   
 
select :: [a] -> [(a,[a])]
select [] = []
select (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- select xs]



tails' :: [a] -> [[a]]
tails' [] = []         -- orignal tails then ... = [[]]
tails' (x:xs) = (x : xs) : tails' xs