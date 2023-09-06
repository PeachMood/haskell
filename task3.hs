module Lists
(get'
,head'
,last'
,tail'
,init'
,reverse'
,length'
,append'
,concat'
,drop'
,take'
,splitAt'
,null'
,elem'
,filter'
,map'
,zip'
,remove'
)where

get' :: (Eq b, Num b) => [a] -> b -> a
get' [] _ = error "Error"
get' (x:xs) n = if n==0 then x else get' xs (n-1)

head' :: [a] -> a
head' [] = error "the empty list"
head' (x:_) = x

last' :: [a] -> a
last' [] = error "the empty list"
last' [x] = x
last' (_:xs) = last' xs

tail':: [a] -> [a]
tail' [] = error "the empty list"
tail' (_:xs) = xs

init' :: [a] -> [a]
init' [] = error "the empty list"
init' [x] = []
init' (x:xs) = x : init' xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = f xs []
    where f [] ys = ys
          f (x:xs) ys = f xs (x:ys)

length' :: Num b => [a] -> b
length' [] = 0
length' (_:xs) = length' xs + 1

append' :: [a] -> a -> [a]
append' [] x = [x]
append' (y:ys) x = y : (append' ys x)

concat' :: [a] -> [a] -> [a]
concat' xs [] = xs
concat' [] ys = ys
concat' xs (y:ys) = concat' (append' xs y) ys

drop' :: (Ord b, Num b) => b -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs)
          | n<=0 = x:xs
          | n==1 = xs
          | otherwise = drop' (n-1) xs

take' :: (Ord b, Num b) => b -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
          | n<=0 = []
          | n==1 = [x]
          | otherwise = x : take' (n-1) xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take' n xs, drop' n xs)

null' :: [a] -> Bool
null' [] = True
null' _ = False

elem' :: Eq a => [a] -> a -> Bool
elem' [] _ = False
elem' (x:xs) y = if x==y then True else elem' xs y

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' test (x:xs)
          |test x = x : filter' test xs
          |otherwise = filter' test xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

remove' _ [] = []
remove' key ((a_key, val):other) = if a_key == key then other else (a_key, val):remove' key other
