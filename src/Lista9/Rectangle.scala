package Lista9

class Rectangle(val a: Double, val b: Double):
  
  def area: Double = a * b

  def this(a: Double) = this(a, a)



object Obj extends App:
  var rect: Rectangle = Rectangle(5.0)
  var rect2: Rectangle = Rectangle(2.0, 3.0)

  println(rect.area)
  println(rect2.area)