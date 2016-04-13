(* Dan Grossman, CSE341, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun find_option (s, ss) =
  let fun iter (ss, n) =
    case ss of
        [] => NONE
      | s'::ss' => if same_string(s, s')
                    then SOME(n)
                    else iter(ss', n+1)
  in
    iter(ss, 0)
  end

fun contains (s, ss) =
  case ss of
       [] => false
     | s'::ss' => if same_string(s, s')
                  then true
                  else contains (s, ss')

fun contains2 (s, ss) =
  let val result = find_option(s, ss)
  in
    case result of
         NONE     => false
       | SOME (n) => true
  end

fun remove (s, ss) =
  case ss of
       []       => []
     | s'::ss'  =>
        if same_string(s, s')
        then
          remove(s, ss')
        else
          s'::remove(s, ss')

fun remove_option (s, ss) =
  if contains (s, ss)
  then SOME(remove (s,ss))
  else
    NONE

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = rank * suit

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
