// Zadanie 1
def revNComp[A](f: A => A)(n: Int)(x: A) =
  def revNComp_iter[A](f: A => A, n: Int, x: A, accum: List[A]): List[A] =
    if n <= 0 then accum
    else revNComp_iter(f, n - 1, f(x), x :: accum)
  revNComp_iter(f, n, x, Nil)

revNComp ((a: Int) => a * 2) (5) (1)
revNComp ((a: Double) => a * a) (5) (2.0)

// Zadanie 2
def area(a: Double, b: Double)(f: Double => Double)(n: Int) =
  def listing(iter: Int): List[Int] =
    if iter < n then iter :: listing(iter + 1)
    else Nil
  val diff = (b - a) / (n - 1)
  val xs = listing(1)

  (xs map (x => f(a + diff * x) * diff) foldLeft(0.0))((sum, x) => sum + x)

area (1.0, 4.0) (x => x*x) (2)
area (0.0, 1.0) (x => x*x*x) (1000)

