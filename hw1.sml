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

(* Accessory function *)
(* Reverses a given list. *)
fun reverse(lst: 'a list) =
  let fun loop(lst: 'a list, out: 'a list) =
    if null lst
    then out
    else loop(tl lst, (hd lst)::out)
  in
    loop(lst, [])
  end

(* Exercise 4 *)
(* Takes a list of dates and a month and returns a list of dates containing that month.
 * Dates are returned in the order provided. *) 
fun dates_in_month (dates: (int*int*int) list, month: int) = 
  let fun loop(dates: (int*int*int) list, matches: (int*int*int) list) =
         if null dates
         then reverse matches
         else
              if (#2 (hd dates)) = month
              then loop(tl dates, (hd dates)::matches)
              else loop(tl dates, matches)
  in
     loop(dates, [])    
  end

(* Exercise 5 *)
(* Takes a list of dates and a list of months and returns a list of dates
* corresponding to those months, in order provided. *)
fun dates_in_months(dates: (int*int*int) list, months: int list) =
  let fun loop(months: int list, matches: (int*int*int) list) =
    if null months
    then matches
    else
        loop(tl months, matches @ dates_in_month(dates, hd months))
  in
    loop(months, [])
  end

(* Exercise 6 *)
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

(* Exercise 7 *)
(* Converts a date to a string *)
fun date_to_string (date: (int*int*int)) =
  let val month_names = ["January", "February", "March", "April", "May", "June",
                   "July", "August", "September", "October", "November", "December"]

      val month = get_nth(month_names, #2 date)
      val date_str = month ^ "-" ^ Int.toString(#1 date) ^ "-" ^ Int.toString(#3
      date)
  in date_str
  end

(* Exercise 8 *)
(* Returns the index n at which the sum of the first n numbers is less
* than a provided threshold, and the sum of the n+1 elements exceed the
* threshold *)
fun number_before_reaching_sum (sum: int, xs: int list) =
  let fun loop(sum_so_far: int, n: int, xs: int list) =
    if sum_so_far > sum
    then n-1
    else
         loop(sum_so_far + (hd xs), n+1, tl xs)
  in
    loop(0, 1, xs)
  end

fun get_month (n: int) =
  let val month_names = ["January", "February", "March", "April", "May", "June",
                   "July", "August", "September", "October", "November", "December"]

  in    
     get_nth(month_names, n)
  end

(* Exercise 9 *)
(* Given a day of the year, return the month *)
fun what_month (day: int) = true
