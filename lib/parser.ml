open Angstrom

let u_dec : int t =
  let* digits = take_while1 (function '0'..'9' -> true | _ -> false) in
  return @@ int_of_string digits

module GridMap = Map.Make(struct
  type t = int * int
  let compare = compare
end)

let grid_of (p : 'a option t) : 'a GridMap.t t =
  failwith "todo"
