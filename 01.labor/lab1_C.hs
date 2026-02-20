import Data.Int (Int)
import Text.XHtml.Transitional (base, abbr, background, caption)
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