import Data.Char ( ord )
import Data.List (intercalate)
import Lists (append', get', remove')

type HashKey = Int
data HashTable k v = HashTable {values ::[[(k,v)]], loadfactor:: Double, size:: Int, capacity:: Int} deriving Show

hashing :: (Show k) => k -> HashKey
hashing key = foldl (\acc ch -> ord ch + acc) 0 (show key)

--Even Hoogle does not know what flatten function is
rehashing :: (Eq k, Show k)=> [[(k,v)]] -> HashTable k v -> HashTable k v
rehashing list hashTable = foldl (\acc (k,v) -> insert k v acc) (realloc $ clear hashTable) (intercalate [] list)
    where realloc HashTable {values = val , loadfactor = lf, size = s, capacity = c} = HashTable {values = val ++ replicate c [] , loadfactor = lf, size = s, capacity = 2*c}

defaultHashTable :: HashTable k v
defaultHashTable = HashTable {values = replicate 20 [], loadfactor = 0.0, size = 0, capacity = 20}

fromList :: (Eq k, Show k) => [(k,v)]-> HashTable k v
fromList = foldl (\acc (k,v) -> insert k v acc) defaultHashTable

clear :: HashTable k v -> HashTable k v
clear HashTable { capacity = c} = HashTable {values = replicate c [], loadfactor = 0.0, size = 0, capacity = c}

erase::(Eq k, Show k)=> HashTable k v-> k ->HashTable k v
erase HashTable {values = val , loadfactor = lf, size = s, capacity = c} k
    | not (contains HashTable {values = val , loadfactor = lf, size = s, capacity = c} k) = HashTable {values = val , loadfactor = lf, size = s, capacity = c}
    | otherwise = HashTable {values = take hashKey val ++ remove' k (get' val hashKey) : tail (drop hashKey val), loadfactor = (fromIntegral s - 1) / fromIntegral c, size = s-1, capacity = c}
    where hashKey = hashing k `mod` c

insert ::(Eq k, Show k) => k -> v -> HashTable k v ->HashTable k v
insert k v HashTable {values = val , loadfactor = lf, size = s, capacity = c}
    | contains HashTable {values = val , loadfactor = lf, size = s, capacity = c} k = insert k v $ erase HashTable {values = val , loadfactor = lf, size = s, capacity = c} k
    | lf >= 1.0 = insert k v (rehashing val HashTable {values = val , loadfactor = lf, size = s, capacity = c})
    | otherwise = HashTable {values = take hashKey val ++ (append' (get' val hashKey) (k,v) : tail (drop hashKey val)), loadfactor = (fromIntegral s + 1)/ fromIntegral c, size = s+1, capacity = c}
    where hashKey = hashing k `mod` c

contains::(Eq k, Show k)=> HashTable k v -> k -> Bool;
contains HashTable {values = val, capacity = c} k = k `elem` map fst (get' val (hashing k `mod` c))

at :: (Eq k, Show k) => HashTable k v -> k -> Maybe v
at HashTable {values = val, capacity = c} k = lookup k (get' val (hashing k `mod` c))

empty::(Eq k, Show k) => HashTable k v -> Bool
empty HashTable {size = s} = s==0

sizeTable::(Show k) => HashTable k v -> Int;
sizeTable HashTable {size = s} = s
