fun map (f,xs) =
  case xs of
       [] => []
     | x::xs' => (f x)::(map(f, xs'))

val names = [("Tom", "Farrell"), ("Dameon", "Johnson"),
    ("Robert", "McDonald")]

fun concat (first: string, last: string) =
  first ^ " " ^ last

val fullNames = map(concat, names)

(* Filter *)
fun filter (f, xs) =
  case xs of
       [] => []
     | x::xs' => if f x
                 then x::(filter(f, xs'))
                 else filter(f, xs')

(* Fold *)
fun fold (f, acc, xs) =
  case xs of
       [] => acc
     | x::xs => fold(f, f(acc,x), xs)

(* No private data *)
fun f1 xs = fold((fn (x,y) => x+y), 0, xs)
fun f2 xs = fold((fn (x,y) => x andalso y>=0), true, xs)

val nums = [1, 2, 3, 4, 5]
val nums2 = [~1, 1, 2, 3, 4]

val sum = f1
val predicateHolds = f2

(* Implement max as a fold *)
fun greaterOf (a,b) =
  if a >= b
  then a
  else b

fun max xs =
  case xs of
       [] => NONE
     | x::[] => SOME(x)
     | x::xs => SOME(fold(greaterOf, hd xs, xs))


