car lst = head lst
caar lst = (car (car lst))
cdr lst = tail lst
cadr lst = (car (cdr lst))
caddr lst = (car (cdr (cdr lst)))
cddr lst = (cdr (cdr lst))

-- findlast lst =
--   if (cdr lst) == []
--     then lst
--     else (findlast (cdr lst))
-- P1
findlast [] = []
findlast (x:[]) = x
findlast (x:xs) = findlast xs

-- findlast2 lst =
--   if (cddr lst) == []
--     then lst
--     else (findlast2 (cdr lst))
-- P2
findlast2 [] = []
findlast2 (x:[]) = [x]
findlast2 (x1:x2:[]) = [x1,x2]
findlast2 (x:xs) = findlast2 xs

-- P3
nth lst n =
  if n == 1
    then (car lst)
    else nth (cdr lst) (n-1)

-- P4
elemcount lst =
  if (null lst)
    then 0
    else 1 + (elemcount (cdr lst))

-- P5
listreverse' l1 l2 c =
  if c > 0
    then
      listreverse' (cdr l1) ((car l1):l2) (c-1)
    else l2
listreverse lst =
  listreverse' lst [] (elemcount lst)

-- P6
palindrome lst =
  lst == (listreverse lst)

-- Credit josuf107 @ r/haskellquestions
-- Types means typing more, no?
-- P7
data Tree a = Tree [Tree a] | Atom a deriving Show
flatten (Tree []) = []
flatten (Tree (x:xs)) = flatten x ++ flatten (Tree xs)
flatten (Atom a) = [a]
-- eg. flatten (Tree [Atom 2, Atom 5, Tree [Atom 5, Atom 3]])

-- P8
compress' l1 l2
  | null l1 = l2
  | elem (car l1) l2 = compress' (cdr l1) l2
  | otherwise = compress' (cdr l1) (l2 ++ [car l1])
compress lst = compress' lst []

-- P9
pack' [] l2 = l2
pack' (x:xs) [] = pack' xs [[x]]
pack' (x1:x1s) [(x2:x2s)] | x1 == x2 = pack' x1s [(x1:x2:x2s)]
pack' (x:xs) l =
  if x == (head (last l)) then
    pack'
      xs
      ((init l) ++ [(last l) ++ [x]])
    else pack' xs (l ++ [[x]])
pack l = pack' l []

-- P10
encode' [] = []
encode' (l:ls) =
  [((length l), (car l))] ++ (encode' ls)
encode lst = encode' (pack lst)

-- P12
uncompress' [] l = l
uncompress' (x:xs) l =
  uncompress' xs (l ++ (take (head x) (repeat (last x))))
uncompress lst = uncompress' lst []

-- P13
encodedirect' [] lst = lst
encodedirect' (x:xs) [] = encodedirect' xs [(1,x)]
encodedirect' (x:xs) lst@(l:ls) =
  if x == (snd l)
    then encodedirect' xs ((1+(fst l) , (snd l)) : ls)
    else encodedirect' xs ((1,x) : lst)
encodedirect lst = reverse (encodedirect' lst [])

-- P14
dupli' [] lst = lst
dupli' (x:xs) lst = dupli' xs (x:x:lst)
dupli lst = reverse (dupli' lst [])
