def find[A](xs:List[A])(x:A): Boolean =
  xs match {
    case h::t if h == x => true
    case Nil => false
    case h::t => find(t)(x)
  }

val find123 = find(List(1, 2, 3))
find123(3)
find123(4)
val findabc = find(List('a', 'b', 'c'))
findabc('a')
findabc('d')

def split2Rec[A](xs: List[A]): (List[A], List[A]) =
  xs match {
    case Nil => (Nil, Nil)
    case h1::h2::t =>
      val (l1, l2) = split2Rec(xs)
      (h1::l1, h2::l2)
    case h1::t => (List(h1), Nil)
  }
split2Rec(List(1))
split2Rec(List(1, 2))
split2Rec(List(1, 2, 3))
split2Rec(List('a', 'b', 'c'))


def split2Tail[A](xs: List[A]) =
  def split2Tail_iter[A](xs:List[A], x:A, xs1: List[A], xs2: List[A]): (List[A], List[A]) =
    (xs, xs1, xs2) match {
      case (h1::t1, Nil, Nil) => split2Tail_iter(t1, x, h1::xs1, xs2)
      case (h1::t1, h2::t2, Nil) if t1 == Nil => split2Tail_iter(t1, x, xs1, h1::xs2)
      case (h1::t1, h2::t2, Nil) => split2Tail_iter(t1, t1.head, xs1, h1::xs2)
      case (h1::t1, h2::t2, h3::t3) if h1 == x => split2Tail_iter(t1, x, h1::xs1, xs2)
      case (h1::t1, h2::t2, h3::t3) if t1 != Nil => split2Tail_iter(t1, t1.head, xs1, h1::xs2)
      case (h1::t1, h2::t2, h3::t3) if h1 != x => split2Tail_iter(t1, x, xs1, h1::xs2)
      case (Nil, _, _) => (xs1, xs2)
    }
  split2Tail_iter(xs, xs.head, Nil, Nil)

split2Tail(List('a', 'b', 'c'))
split2Tail(List(1))
split2Tail(List(1, 2))
split2Tail(List(1, 2, 3))
split2Tail(List(1, 2, 3, 4))
split2Tail(List(1, 2, 3, 4, 5, 6))