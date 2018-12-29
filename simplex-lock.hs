import Data.List (permutations, intersect)

digits = [1..5]

-- Return all k combinations of (x:xs)
combinations :: Int -> [b] -> [[b]]
combinations 0 _  = [[]]
combinations _ []  =  []
combinations k (x:xs)  = map (x:) (combinations (k-1) xs) ++ combinations k xs

-- Return elements of xs that appear after x.
after xs x = case dropWhile (/= x) xs of
  [] -> []
  x:xs -> xs

-- Return elements of xs that are not in ys.
diff xs ys = [ x | x <- xs, not $ elem x ys]

disjoint xs ys = null $ intersect xs ys

-- Chain d with each remaining element from xs.
-- d can be a double, triple, quadruple, quintuple, etc
-- invariant property of tuples: descending order
chain xs d = map (\a -> d ++ [a]) ys
  where
    ys = after xs (last d)

-- Given a tuple-generating function, return a function that generates the next level of tuples.
next tuples = \xs -> concat $ map (chain xs) (tuples xs)

-- Elevate tuple-generating function to the nth order.
elevate n tuples =
  if n <= 0 then tuples
  else elevate (n - 1) (next tuples)

ntuples n = elevate (n - 1) singles

-- Return all singletons.
singles = map (\a -> [a])

doubles [] = []
doubles (x:xs) = ds ++ (doubles xs)
  where
    ds = map (\b -> x:[b]) xs
-- doubles = next singles
-- doubles = ntuples 2

triples [] = []
triples xs = concat $ map (chain xs) (doubles xs)
-- triples = next doubles
-- triples = next $ next singles
-- triples = ntuples 3

quadruples [] = []
quadruples xs = concat $ map (chain xs) (triples xs)
-- quadruples = next triples
-- quadruples = next $ next $ next singles
-- quadruples = ntuples 4

quintuples [] = []
quintuples xs = concat $ map (chain xs) (quadruples xs)
-- quintuples = next quadruples
-- quintuples = next $ next $ next $ next singles
-- quintuples = ntuples 5


-- All codes that use exactly 5 buttons
b5 = concat $ map (concat . map permutations)
  [b5s11111, b5s1112, b5s122, b5s113, b5s23, b5s14, b5s5]

b5s11111 = [ singles digits ]
b5s1112  = [ d : (singles $ diff digits d) | d <- doubles digits ]
-- FIXME inefficient
b5s122 = [ d0 : d1 : (singles $ diff digits (d0 ++ d1)) | d0 <- ds, d1 <- after ds d0, disjoint d0 d1 ]
  where ds = doubles digits
b5s113 = [ t : (singles $ diff digits t) | t <- triples digits ]
b5s23 = [ [d, t] | d <- doubles digits, t <- triples (diff digits d) ]
b5s14 = [ [q, s] | q <- quadruples digits, s <- singles (diff digits q) ]
b5s5 = [ quintuples digits ]


-- All codes that use exatly 4 buttons
b4 = concat $ map (concat . map permutations)
  [b4s1111, b4s112, b4s22, b4s13, b4s4]

b4s1111 = combinations 4 (singles digits)
b4s112 = [ d : (singles $ diff c d) | c <- combinations 4 digits, d <- doubles c ]
b4s22 = [ d0 : [d1] | c <- combinations 4 digits, d0 <- doubles c, d1 <- after (doubles c) d0, disjoint d0 d1 ]
b4s13 = [ [t, s] | c <- combinations 4 digits, t <- triples c, s <- singles (diff c t) ]
b4s4 = [ [q] | c <- combinations 4 digits, q <- quadruples c ]

-- All codes that use eactly 3 buttons
b3 = concat $ map (concat . map permutations)
  [b3s111, b3s12, b3s3]

b3s111 = combinations 3 (singles digits)
b3s12 = [ [d, s] | c <- combinations 3 digits, d <- doubles c, s <- singles $ diff c d ]
b3s3 = [ [t] | c <- combinations 3 digits, t <- triples c ]

-- All codes that use eactly 2 buttons
b2 = concat $ map (concat . map permutations)
  [b2s11, b2s2]

b2s11 = combinations 2 (singles digits)
b2s2 = [ [d] | c <- combinations 2 digits, d <- doubles c ]

-- All codes that use eactly 1 button
b1 = [ [s] | s <- singles digits ]

-- not included: null combination (door is always open)
codes = b1 ++ b2 ++ b3 ++ b4 ++ b5


