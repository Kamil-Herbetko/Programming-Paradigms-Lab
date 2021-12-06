// Kamil Herbetko

// Zadanie 1
sealed trait tree3[+A]
  case object Empty extends tree3[Nothing]
  case class Node[+A](elem: A, bt_1: tree3[A], bt_2: tree3[A], bt_3: tree3[A]) extends tree3[A]

val intTree3 = Node(1,
                    Node(2,
                        Empty,
                        Node(3,
                            Empty,
                            Empty,
                            Empty
                            ),
                        Empty
                        ),
                    Node(4,
                        Node(5,
                            Empty,
                            Empty,
                            Empty
                            ),
                        Empty,
                        Empty
                        ),
                    Node(6,
                        Empty,
                        Empty,
                        Node(7,
                            Empty,
                            Empty,
                            Empty
                            )
                        )
              );

val sTree3 = Node("a",
                  Node("b",
                        Empty,
                        Node("c",
                              Empty,
                              Empty,
                              Empty
                            ),
                        Empty),
                  Node("d",
                        Node("e",
                              Empty,
                              Empty,
                              Empty
                            ),
                        Empty,
                        Empty
                      ),
                  Node("f",
                        Empty,
                        Empty,
                        Node("g",
                              Empty,
                              Empty,
                              Empty
                            )
                      )
                  );

def mapTree3[A](f: A => A)(bt: tree3[A]): tree3[A] = {
  bt match {
    case Empty => Empty
    case Node(elem, bt_1, bt_2, bt_3) => Node(f(elem), mapTree3(f)(bt_1), mapTree3(f)(bt_2), mapTree3(f)(bt_3))
  }
}


mapTree3((x: Int) => x * x)(intTree3)
mapTree3((x: Int) => x * x)(Empty)
mapTree3((x: String) => x + x + x)(sTree3)

case class Word(first_letter: Char, rest_of_word: String)

sealed trait Sentence
case class Declarative(word: Word, words: List[Word]) extends Sentence
case class Exclamatory(word: Word, words: List[Word]) extends Sentence
case class Interrogative(word: Word, words: List[Word]) extends Sentence

case class Text(first_sentence: Sentence, rest_of_sentences: List[Sentence])

val (sKot: Sentence) = Declarative(Word('a', "la"), List(Word('m', "a"), Word('k', "ota")))
val (sRys: Sentence) = Interrogative(Word('a', ""), List(Word('s', "ierotka"), Word('m', "a"), Word('r', "ysia")))
val (sGadasz: Sentence) = Exclamatory(Word('n', "ie,"), List(Word('c', "o"), Word('t', "y"), Word('g', "adasz")))
val (txt: Text) = Text(sKot, List(sRys, sGadasz))