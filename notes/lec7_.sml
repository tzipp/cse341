fun map (f,xs) =
  case xs of
       [] => []
     | x::xs' => (f x)::(map(f, xs'))

val names = [("Tom", "Farrell"), ("Dameon", "Johnson"),
    ("Robert", "McDonald")]

fun concat (first: string, last: string) =
  first ^ " " ^ last

val fullNames = map(concat, names)


