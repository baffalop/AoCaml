open Angstrom

let u_dec : int t =
  let* digits = take_while1 (function '0'..'9' -> true | _ -> false) in
  return @@ int_of_string digits
