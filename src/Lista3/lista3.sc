// Zadanie 1
def revNComp[A](f: A => A)(n: Int)(x: A) =
  def revNComp_iter[A](f: A => A, n: Int, x: A, accum: List[A]): List[A] =
    if n <= 0 then accum
    else revNComp_iter(f, n - 1, f(x), x :: accum)
  revNComp_iter(f, n, x, Nil)

revNComp ((a: Int) => a * 2) (5) (1)
revNComp ((a: Double) => a * a) (5) (2.0)

// Zadanie 2
def listing(a: Double, b: Double, n: Int, first_a: Double, iterator: Int): List[(Double, Double)] =
  if iterator == 1 then (a, 0) :: (listing(a + ((b - first_a) / (n - 1)), b, n, first_a, iterator + 1))
  else if (iterator < n) then (a, ((b - first_a) / (n - 1))) :: (listing(a + ((b - first_a) / (n - 1)), b, n, first_a, iterator + 1))
  else List((b, ((b - first_a) / (n - 1))))

def area(a: Double, b: Double)(f: Double => Double)(n: Int) =
  def listing(iter: Int): List[Int] =
    if iter < n then iter :: listing(iter + 1)
    else Nil
  val diff = (b - a) / (n - 1)
  val xs = listing(1)

  (xs map (x => f(a + diff * x) * diff) foldLeft(0.0))((sum, x) => sum + x)

area (1.0, 4.0) (x => x*x) (2)
area (0.0, 1.0) (x => x*x*x) (1000)

