car lst = head lst
cdr lst = tail lst
cadr lst = (car (cdr lst))
caddr lst = (car (cdr (cdr lst)))
cddr lst = (cdr (cdr lst))

nil lst = (cdr lst) == []

findlast lst =
  if (cdr lst) == []
    then lst
    else (findlast (cdr lst))

findlast2 lst =
  if (cddr lst) == []
    then lst
    else (findlast2 (cdr lst))

elemcount lst =
  if (nil (cdr lst))
    then 1
    else 1 + (elemcount (cdr lst))
