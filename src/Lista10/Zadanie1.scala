package Lista10

object Zadanie1 extends App:
  var lista_anyref: List[AnyRef] = List(new AnyRef(), new AnyRef(), new AnyRef())
  var lista_string: List[String] = List("", "", "")

  var lista_anyref2: List[AnyRef] = List(new AnyRef(), new AnyRef(), new AnyRef())
  var lista_string2: List[String] = List("", "", "")

  println(lista_anyref)
  println(lista_string)

  lista_anyref = lista_string
  lista_anyref = "" :: lista_anyref
  
  //lista_string2 = lista_anyref2 

  println(lista_anyref)