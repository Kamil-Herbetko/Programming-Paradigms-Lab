// Kamil Herbetko

// Zadanie 2
def sublistsL[A](lazyList: LazyList[A]): LazyList[List[A]] =
  def makeList(lazyList: LazyList[A], count: Int): List[A] =
    (lazyList, count) match
      case (h #:: t, 1) => List(h)
      case (h #:: t, n) => h :: makeList(t, count - 1)
      case (LazyList(), _) => Nil

  def sublistsL_iter(lazyList: LazyList[A], lazyhelper: LazyList[A], counter: Int): LazyList[List[A]] =
    (lazyList, lazyhelper, counter) match
      case (LazyList(), _, _) => LazyList()
      case (h #:: t, LazyList(), _) => LazyList()
      case (h #:: t, _ #:: thlp, n) => (makeList(lazyList, n)) #:: (sublistsL_iter(lazyList, thlp, n + 1))

  sublistsL_iter(lazyList, lazyList, 1)

sublistsL(LazyList(1, 2, 3, 4)).force
sublistsL(LazyList.from(1)).take(15).toList
sublistsL(LazyList())

