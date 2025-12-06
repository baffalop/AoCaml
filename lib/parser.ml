open Angstrom

let (<<) f g x = f (g x)

let is_digit : char -> bool = function
  | '0'..'9' -> true
  | _ -> false

let digit : int t =
  int_of_string << Char.escaped <$> satisfy is_digit

let u_dec : int t =
  let* digits = take_while1 is_digit in
  return @@ int_of_string digits

let lines_of (p : 'a t) : 'a list t =
  sep_by1 (char '\n') p
