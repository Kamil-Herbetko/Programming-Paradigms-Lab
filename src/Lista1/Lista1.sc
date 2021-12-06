// Kamil Herbetko

// Zadanie 1
def reverse4[A,B,C,D](x:(A,B,C,D)) =
  (x._4, x._3, x._2, x._1)

reverse4((6, 6.0, "ala", true)) == (true, "ala", 6.0, 6)

// Zadanie 2
def substitute[A](xs : List[A], x: A, y:A) : List[A] =
  if xs == Nil then Nil
  else if xs.head == x then y :: substitute(xs.tail, x, y)
  else xs.head :: substitute(xs.tail, x, y)

substitute(List(1, 2, 3, 4, 5, 6, 5, 5, 7), 5, 1) == List(1, 2, 3, 4, 1, 6, 1, 1, 7)
substitute(List(1, 2, 3), 5, 3) == List(1, 2, 3)
substitute(Nil, 2, 3) == Nil
substitute(List('a', 'b', 'c', 'c', 'd', 'e', 'e'), 'c', 'g') == List('a', 'b', 'g', 'g', 'd', 'e', 'e')

// Zadanie 3
def insert[A](xs: List[A], x: A, y: Int): List[A] =
  if y < 1 then x :: xs
  else if xs == Nil then List(x)
  else xs.head :: insert(xs.tail, x, y-1)

insert(List(1, 2, 3, 4), 5, 1) == List(1, 5, 2, 3, 4)
insert(List(1, 2, 3, 4), 5, 7) == List(1, 2, 3, 4, 5)
insert(List('a', 'b', 'c'), 'd', -3) == List('d', 'a', 'b', 'c')
insert(Nil, 1, 2) == List(1)
