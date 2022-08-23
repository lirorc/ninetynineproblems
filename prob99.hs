car lst = head lst
cdr lst = tail lst
cadr lst = (car (cdr lst))
caddr lst = (car (cdr (cdr lst)))
cddr lst = (cdr (cdr lst))

findlast lst =
  if (cdr lst) == []
    then lst
    else (findlast (cdr lst))

findlast2 lst =
  if (cddr lst) == []
    then lst
    else (findlast2 (cdr lst))

nth lst n =
  if n == 1
    then (car lst)
    else nth (cdr lst) (n-1)

elemcount lst =
  if (null lst)
    then 0
    else 1 + (elemcount (cdr lst))

listreverse' l1 l2 c =
  if c > 0
    then
      listreverse' (cdr l1) ((car l1):l2) (c-1)
    else l2
listreverse lst =
  listreverse' lst [] (elemcount lst)

palindrome lst =
  lst == (listreverse lst)


-- Credit josuf107 @ r/haskellquestions
-- Types means typing more, no?
data Tree a = Tree [Tree a] | Atom a deriving Show
flatten (Tree []) = []
flatten (Tree (x:xs)) = flatten x ++ flatten (Tree xs)
flatten (Atom a) = [a]
-- eg. flatten (Tree [Atom 2, Atom 5, Tree [Atom 5, Atom 3]])

compress' l1 l2
  | l1 == [] = l2
  | elem (car l1) l2 = compress' (cdr l1) l2
  | otherwise = compress' (cdr l1) (l2 ++ [car l1])
compress lst = compress' lst []
