let year = 2025
let day = 2

open Import

module Range = struct
  type t = int * int

  let show (a, b) = Printf.sprintf "%d-%d (%d)" a b (b - a)

  let show_list : t list -> string = String.concat ", " << List.map show
end

module Parse : sig
  val parse : string -> (Range.t list, string) result
end = struct
  open Angstrom

  let range : Range.t t =
    let* a = Parser.u_dec <* char '-' in
    let* b = Parser.u_dec in
    return (a, b)

  let parse : string -> (Range.t list, string) result =
    parse_string ~consume:Prefix @@ sep_by1 (char ',') range
end

let int_len (n : int) : int =
  int_of_float (log10 @@ float_of_int n) + 1

let half_fl (n : int) : int =
  let len = float_of_int @@ int_len n in
  let half_len = ceil @@ len /. 2. in
  let exp = int_of_float @@ 10. ** half_len in
  n / exp

let half_ceil (n : int) : int =
  let len = float_of_int @@ int_len n in
  let half_len = floor @@ len /. 2. in
  let exp = int_of_float @@ 10. ** half_len in
  n / exp

let double (n : int) : int =
  let len = int_len n in
  let exp = int_of_float @@ 10. ** float_of_int len in
  n * exp + n

module Solution(Part : sig
  val invalids : Range.t -> int list
end) : sig
  val run : string -> (string, string) result
end = struct
  let run =
    Parse.parse
    >> Result.map (List.concat_map Part.invalids
      >> List.fold_left (+) 0
      >> string_of_int)
end

module Part_1 = Solution(struct
  let invalids ((a, b) : Range.t) : int list =
    let half_b = half_ceil b in
    let first_half = ref @@ half_fl a in
    let invalids = ref [] in

    while !first_half <= half_b do
      let candidate = double !first_half in
      if candidate >= a && candidate <= b then
        invalids := candidate :: !invalids;
      incr first_half;
    done;

    !invalids
end)

module Part_2 : sig
  val run : string -> (string, string) result
end = struct
  let run (input : string) : (string, string) result = failwith "not implemented"
end
