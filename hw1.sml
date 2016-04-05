(* Exercise 1 *)
(* Determines whether the first date provided is older than the second date
* provided.*)
fun is_older (date1: int*int*int, date2: int*int*int) =
  if (#3 date1) < (#3 date2)
  then true
  else if (#2 date1) < (#2 date2)
       then true
       else if (#1 date1) < (#1 date1)
            then true
            else false

(* Exercise 2 *)
(* Returns the number of dates falling in a given month. *)
fun number_in_month(dates:(int*int*int) list, month:int) =
  let fun loop(dates:(int*int*int) list, count:int) =
          if null dates
          then count
          else
              if (#2 (hd dates)) = month
              then loop(tl dates, count+1)
              else loop(tl dates, count)
  in
      loop(dates, 0)
  end

(* Exercise 3 *)
(* Returns the number of dates falling in the given months *)
fun number_in_months (dates: (int*int*int) list, months: int list) =
  let fun loop(months: int list, count: int) =
    if null months
    then count
    else
         loop(tl months, count + number_in_month(dates, hd months))
  in
      loop(months, 0)
  end

(* Exercise 5 *)
(* Returns the nth string in a list of strings. *)
fun get_nth (ss: string list, n: int) =
  let fun loop(ss: string list, count: int) =
    if null ss
    then "Not found or empty!"
    else
        if count=n
        then hd ss
        else
            loop(tl ss, count+1)
  in
      loop(ss, 1)
  end


