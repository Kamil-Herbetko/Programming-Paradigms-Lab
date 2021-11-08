(* Kamil Herbetko *)

(* Zadnaie 1 *)
type 'a tree3 = Empty | Node of 'a * 'a tree3 * 'a tree3 * 'a tree3;;

let rec mapTree3 f = function
    | Empty -> Empty
    | Node(x, t1, t2, t3) -> Node(f x, mapTree3 f t1, mapTree3 f t2, mapTree3 f t3);;

let intTree3 =Node(1,
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
                            Empty)
                        )
            );;
let sTree3 =Node("a",
                Node("b",
                    Empty,
                    Node("c",
                        Empty,
                        Empty,
                        Empty
                        ),
                    Empty
                    ),
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
            );;

mapTree3 (fun x -> x * x) intTree3;;
mapTree3 (fun x -> x ^ x ^ x) sTree3;;

(* Zadanie 2 *)
type word = Word of char * string;;

type sentence = Declarative of word * word list | Exclamatory of word * word list | Interrogative of word * word list;;

type text = Text of sentence * sentence list;;

let char_to_string = String.make 1;;

let capital = function
    | Word(chr, str) -> (char_to_string (Char.uppercase chr)) ^ str;;

let word_to_string = function
    | Word(chr, str) -> " " ^ char_to_string (chr) ^ str ;;

let sentenceToString = function
    | Declarative(x, xs) -> capital (x) ^ (List.fold_left (fun acc -> fun x -> acc ^ word_to_string x) "" xs) ^ "."
    | Exclamatory(x, xs) -> capital (x) ^ (List.fold_left (fun acc -> fun x -> acc ^ word_to_string x) "" xs) ^ "!"
    | Interrogative(x, xs) -> capital (x) ^ (List.fold_left (fun acc -> fun x -> acc ^ word_to_string x) "" xs) ^ "?";;

let textToString = function
    | Text(x, xs) -> (sentenceToString x) ^ (List.fold_left (fun acc -> fun x -> acc ^ " " ^ sentenceToString x) "" xs);;

let sKot= Declarative(Word('a', "la"), [Word('m', "a"); Word('k', "ota")]);;
let sRys= Interrogative(Word('a', ""), [Word('s', "ierotka"); Word('m', "a"); Word('r', "ysia")]);;
let sGadasz= Exclamatory(Word('n', "ie,"), [Word('c', "o"); Word('t', "y"); Word('g', "adasz")]);;
let txt= Text(sKot, [sRys; sGadasz]);;

sentenceToString sKot;;
textToString txt;;