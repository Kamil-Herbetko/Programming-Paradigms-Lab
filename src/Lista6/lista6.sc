// Kamil Herbetko
// Zadanie 2

def modifiedPascalF(n: Int): List[Int] =
  def modifiedPascalF_iter(i: Int)(list: List[Int]): List[Int] =
    list match
      case List() => List(1)
      case h :: List() => List(1, 1)
      case h :: t => list.zip(t).map(if i % 2 == 0 then n => n._1 + n._2 else n => n._1 - n._2)
  if n == 0 then modifiedPascalF_iter(0)(List())
  else if n == 1 then modifiedPascalF_iter(1)(List(1))
  else modifiedPascalF_iter(n)(modifiedPascalF(n - 1))


modifiedPascalF(0)
modifiedPascalF(1)
modifiedPascalF(2)
modifiedPascalF(3)
modifiedPascalF(4)



def modifiedPascalI(n: Int): Array[Int] =
  var arr = new Array[Int](n + 1);
  var arrHelper = new Array[Int](n + 1);
  arr(0) = 1;
  arrHelper(0) = 1;
  var i = 1;
  while i < n + 1 do {
    if (i == 1) {
      arr(1) = 1
    }
    else if (i % 2 == 0) {
      var j = 1;
      while j < i + 1 do
        arrHelper(j) = arr(j - 1) + arr(j)
        j += 1
      end while
    }
    else {
      var k = 1;
      while k < i + 1 do
        arr(k) = arrHelper(k - 1) - arrHelper(k)
        k += 1
      end while
    }
    i += 1
  }
  if (n % 2 == 0) then arrHelper
  else arr

modifiedPascalI(0)
modifiedPascalI(1)
modifiedPascalI(2)
modifiedPascalI(3)
modifiedPascalI(4)
modifiedPascalI(5)
modifiedPascalI(6)







