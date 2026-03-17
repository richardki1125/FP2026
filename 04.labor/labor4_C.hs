-- # 4. labor

-- I. Definiáljuk azt a Haskell-listát, amely tartalmazza:

-- - az első n páros szám négyzetét,
negyzet n = take n [i^2 | i<-[2,4..]]

negyzet2 n = take n $ map (\i -> i ^ 2) [2,4 ..]

negyzet3 :: Int -> IO ()
negyzet3 n = mapM_ (\(szam,negyzete) -> print (show szam ++ "negyzete" ++ show negyzete)) ls
    where
        ls = take n $ map (\i -> (i,i^2)) [2,4 ..]
-- - az első $$[1, 2, 2, 3, 3, 3, 4, 4, 4, 4,\ldots]$$,

szamokLs 1 = replicate 1 1
szamokLs n = szamokLs (n-1) ++ replicate n n

szamokLs2 n i
    | i /= n = replicate i i ++ szamokLs2 n (i+1)
    | otherwise = replicate i i

szamokLs4 n i
    | i /= n = replicate i (i*2) ++ szamokLs4 n (i+1)
    |otherwise = replicate i (i*2)

szamokLs5 n = [n,n - 1 .. 1] ++ [1 .. n]
-- - az első $$[2, 4, 4, 6, 6, 6, 8, 8, 8, 8\ldots]$$,
-- - az első $$[n, n-1, \ldots, 2, 1, 1, 2, \ldots, n-1, n]$$,
-- - váltakozva tartalmazzon True és False értékeket,
valtakozo n = take n ls
    where
        ls = [True,False] ++ ls

-- - váltakozva tartalmazza a $$0,\ 1,\ -1$$ értékeket.
valtakozo2 n = take n ls
    where
        ls = [0,1, -1] ++ ls

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy adott szám osztóinak számát,
osztok x = [i | i <- [1..x],mod x i == 0]

osztok2 x =  myLenght [i | i <- [1 .. x], mod x i ==0]
    where
        myLenght [] = 0
        myLenght (_ : ls) = 1 + myLenght ls

osztok3 x = fold1 (\res i -> if mod x i == 0 then res +1 else res ) 0 [1 .. x]

osztok4 x = fold1 (\res i -> if mod x i == 0 then res +1 else res) 1[1 .. div x 2]
-- - meghatározza egy adott szám legnagyobb páratlan osztóját,
maxParatlanOsztok n = last [i | i <- [1,3 .. n],mod n i == 0]

maxParatlanOsztok2 n = maximum [i | i <- [1,3 .. n],mod n i == 0]

maxParatlanOsztok3 n = maximum [i | i <- [1 .. n],mod n i == odd i]

maxParatlanOsztok4 n = myMaximum [i | i <- [1,3 .. n],mod n i == 0]
    where
        myMaximum [x] = x
        myMaximum (x1:x2:xs)
            | x1 > x2 = myMaximum (x1 : xs)
            | otherwise = myMaximum (x2 : xs)

-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerben, hány számjegyet tartalmaz,

-- - meghatározza, hogy egy tízes számrendszerbeli szám p számrendszerbeli alakjában melyik a legnagyobb számjegy,
-- - meghatározza az $a$ és $b$ közötti Fibonacci számokat, $a > 50$.

-- III. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista pozitív elemeinek átlagát,
-- - meghatározzuk azt a listát, amely tartalmazza az eredeti lista minden n-ik elemét,
-- - tükrözi egy lista elemeit,
-- - két módszerrel is meghatározza egy lista legnagyobb elemeinek pozícióit: a lista elemeit kétszer járja be, illetve úgy hogy a lista elemeit csak egyszer járja be,
-- - meghatározza egy lista leggyakrabban előforduló elemét.


main = do
    negyzet3 4
    print (negyzet2 4)