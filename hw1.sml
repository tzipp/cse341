fun is_older (date1: int*int*int, date2: int*int*int) =
  if (#3 date1) < (#3 date2)
  then true
  else if (#2 date1) < (#2 date2)
       then true
       else if (#1 date1) < (#1 date1)
            then true
            else false

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


