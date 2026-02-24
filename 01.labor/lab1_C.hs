import Data.Int (Int)
import Text.XHtml.Transitional (base, abbr, background, caption)
import System.Win32 (LOCALESIGNATURE(lsCsbDefault))
--I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza

-- két szám összegét, különbségét, szorzatát, hányadosát, osztási maradékát,
osszeg :: (Num a) => a -> a -> a
osszeg a b = a + b

osszeg2 :: Int -> Int -> Int
osszeg2 a b = (+) a b

kulonbseg :: Num a => a -> a -> a
kulonbseg a b = a - b

kulonbseg2 :: Double -> Double -> Double
kulonbseg2 a b = (-) a b

szorzat :: Num a => a -> a -> a
szorzat a b = a * b

szorzat2 :: Num a => a -> a -> a
szorzat2 a b = (*) a b

hanyados :: Fractional a => a -> a -> a
hanyados a b = a / b

hanyados2 :: Integral a => a -> a -> a
hanyados2 a b = div a b

hanyados3 :: Integral a => a -> a -> a
hanyados3 a b = a `div` b

osztmar2 :: (Integral a) => a -> a -> a
osztmar2 a b = a `mod` b


-- egy első fokú egyenlet gyökét,
elsoF a b = -b / a
-- egy szám abszulút értékét,
abszolut n = if n < 0 then -n else n

abszolut2 n
    | n < 0 = -n
    | otherwise = n
    
-- egy szám előjelét,

szamelojel n = if n < 0 then "neg" else if n > 0 then "poz" else "nulla"

szamelojel2 n
    | n < 0 = "neg"
    | n > 0 = "poz"
    | otherwise = "nulla"
-- két argumentuma közül a maximumot,
max1 a b
    | a > b = a
    | otherwise = b

max2 a b = if a > b then a else b
-- két argumentuma közül a minimumot,
min1 a b
    | a < b = a
    | otherwise = b

min2 a b = if a < b then a else b
--egy másodfokú egyenlet gyökeit,

masodFoku a b c
    | delta < 0 = error "komplex szamok"
    | otherwise = (gy1,gy2)
    where 
      delta = b ** 2 - 4 * a * c
      gy1 = (-b - sqrt delta) / (2*a)
      gy2 = (-b + sqrt delta) / (2*a)

masodFoku3 a b c = if delta < 0 then error "komplex szamok" else (gy1,gy2)
    where 
      delta = b ** 2 - 4 * a * c
      gy1 = (-b - sqrt delta) / (2*a)
      gy2 = (-b + sqrt delta) / (2*a)
-- hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.
  --Például: $$(6, 7)$$ egyenlő $$(7,6)$$-al, de $$(6, 7)$$ nem egyenlő $$(4, 7)$$-el.
elempar (a,b) (c,d) = if (a == c && b == d) || (a == d && b == c) then True else False

elempar2 (a,b) (c,d) = (a == c && b == d) || (a == d && b == c) 

elempar3 ep1 ep2 = (a == c && b == d) || (a == d && b == c) 
  where
    (a,b) = ep1
    (c,d) = ep2
-- az n szám faktoriálisát (3 módszer),
fakt1 0 = 1
fakt1 n = n * fakt1(n - 1)

fakt2 n
  | n < 0 = -1
  | n == 0 = 1
  | otherwise = n * fakt2 (n -1)


-- meghivas fakt3 1 5
fakt3 res n
  | n < 0  || res < 0 = error "negativ bemenet"
  | n == 0 = res
  | otherwise = fakt3 (res * n)(n -1)

-- az x szám n-ik hatványát, ha a kitevő pozitív szám (3 módszer).

hatvanyXN x n
  | n < 0 = error "neg kitevo"
  | otherwise = x ** n

hatvanyXN2 x n
   | n < 0 = error "neg kitevo"
   | otherwise = x ^ n
  
hatvanyXN3 x n
   | n < 0 = error "neg kitevo"
   | n == 0 = 1
   | otherwise = x * hatvanyXN3 x (n-1)
   

-- II. Könyvtárfüggvények használata nélkül, illetve halmazkifejezéseket alkalmazva, definiáljuk azt a függvényt, amely meghatározza:
-- az első n természetes szám negyzetgyökét,
negyzetgyok n = [sqrt i | i <- [1..n]]

-- az első n négyzetszámot,
negyzet n =  [i ^ 2 | i <- [1..n]]
-- az első n természetes szám köbét,
kob n =  [i ^ 3 | i <- [1..n]]
-- az első n olyan természetes számot, amelyben nem szerepelnek a négyzetszámok,
nemnegyzetszamok n = [i | i <- [1..n],(sqrt i * sqrt i) /= i]
-- x hatványait adott n-ig,
xhatvanyai x n = [x^i | i <- [0..n]]
-- egy szám páros osztóinak listáját,
parososztok x = [i | i <- [1..x],mod x i == 0,mod i 2 == 0]
-- n-ig a prímszámok listáját,
osztok x = [i | i <- [1..x],mod x i == 0]

primszam x = osztok x == [1,x]

primszamokN n = [i | i<-[2..n],primszam i]

primszamokN2 n = [i | i<-[2..n],primszamL i]
  where
    primszamL ns = osztokL ns == [1,ns]
    osztokL ns2 = [i | i <- [1..ns2],mod ns2 i == 0]

-- n-ig az összetett számok listáját,
oszetett n = [i | i <- [1..n],primszam i == False]

-- n-ig a páratlan összetett számok listáját,
paratlanoszetett n = [i | i <- [1..n],not(primszam i),mod i 2 /= 0]

paratlanoszetett2 n = [i | i <- [1,3..n],not(primszam i)]
-- az n-nél kisebb Pitágorászi számhármasokat,

pitagorasz n = [(a,b,c) | c <- [1..n],b <- [1..c], a <-[1..b],a ** 2 + b ** 2 == c ** 2]

-- a következő listát: [('a',0), ('b',1),..., ('z', 25)],
betuszlista = zip ['a'..'z'][0..25]

-- a következő listát: [(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)], majd általánosítsuk a feladatot.
szamoklista = zip [0..5][5,4..0]

szamoklista1 n = zip [0..n][n,n-1..0]

szamoklista2 n = [(i,n-i) | i <- [0..n]]

-- azt a listát, ami felváltva tartalmaz True és False értékeket.

truefalselista n = take n ls
  where
    ls = [True,False] ++ ls

truefalselista2 n = [even i | i <- [0..n-1]]