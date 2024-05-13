let valid_date d m =
  if d < 1 then
    false
  else if
    m = "Jan" || m = "Mar" || m = "May" || m = "Jul" || m = "Aug" || m = "Oct"
    || m = "Dec"
  then
    if d <= 31 then
      true
    else
      false
  else if m = "Feb" then
    if d <= 28 || d = 29 then
      true
    else
      false
  else if m = "Apr" || m = "Jun" || m = "Sep" || m = "Nov" then
    if d <= 30 then
      true
    else
      false
  else
    false

let rec fib n =
  if n = 0 then
    0
  else if n = 1 then
    1
  else
    fib (n - 1) + fib (n - 2)

let rec h n previous_previous previous =
  if n = 1 then
    previous
  else
    h (n - 1) previous (previous_previous + previous)

let fib_fast n =
  if n = 0 then
    0
  else
    h n 0 1

let divide ~numerator ~denominator =
    numerator /. denominator

let ( +/. ) x y = (x +. y) /. 2.

let my_array = [1; 2; 3; 4]
