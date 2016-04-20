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

