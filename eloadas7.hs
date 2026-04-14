import Data.List


mySum ls = foldl op 0 ls
    where
        op res k = res + k


--megforditja a listat
myMap fg ls = foldl (op fg) [] ls
    where
        op fg res k = fg k : res

myMap1 fg ls = foldr (op fg) [] ls
    where
        op fg k res = fg k : res

myFilter fg ls = foldr (op fg) [] ls
    where
        op fg k res = if fg k then k : res else res

myElem x ls = foldr (op x) False ls
    where
        op x k res = if x == k then True || res else res

myElem1 x ls = foldl (op x) False ls
    where
        op x res k = if x == k then True || res else res



fgIns x y  = fst x < fst y

fibonacciN2 n = fst $ foldl op (0,1) [1..n]
    where 
        op (a,b) k = (b,a + b)

fibonacciLs1 n = take n $ fibonacciAux (0,1)
    where
        fibonacciAux (a,b) = a : fibonacciAux (b,a + b)

        