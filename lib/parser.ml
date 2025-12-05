open Angstrom

let u_dec : int t =
  let* digits = take_while1 (function '0'..'9' -> true | _ -> false) in
  return @@ int_of_string digits

let lines_of (p : 'a t) : 'a list t =
  sep_by1 (char '\n') p
