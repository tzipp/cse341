datatype mytype = TwoInts of int * int
                | Str of string
                | Pizza

fun f x =
    case x of
         Pizza => 3
       | TwoInts(i1, i2) => i1 + i2
       | Str s => String.size s


datatype ioption = Some of int
                 | None

fun getOrElse (opt: ioption, x: int) =
  case opt of
       Some(i) => i
     | None => x

datatype suit = Club | Diamond | Heart | Spade
datatype card_value = Jack | Queen | King | Ace
                    | Num of int

datatype id = StudentNum of int
            | Name of string
                      * (string option)
                      * string

(* Expression Tree! *)
datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp

fun eval e =
  case e of
       Constant i       => i
     | Negate e2        => ~ (eval e2)
     | Add(e1, e2)      => (eval e1) + (eval e2)
     | Multiply(e1, e2) => (eval e1) * (eval e2)

datatype my_int_list = Empty
                     | Cons of int * my_int_list

val x = Cons(4, Cons(23, Cons(45, Empty)))

fun append_my_list (xs, ys) =
  case xs of
       Empty => ys
     | Cons(x, xs') => Cons(x, append_my_list(xs', ys))

fun headOption (xs) =
  case xs of
       Empty        => NONE
     | Cons(x, xs') => SOME(x)

fun sum_list (xs: my_int_list) =
  case xs of
       Empty        => 0
     | Cons(x, xs') => x + sum_list(xs')

fun sum_triple triple =
  let val (x, y, z) = triple
  in
      x + y + z
  end

fun full_name r =
  let val {first=x, middle=y, last=z} = r
  in
      x ^ " " ^ y ^ " " ^ z
  end


