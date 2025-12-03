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

let digits (n : int) : int =
  int_of_float (log10 @@ float_of_int n) + 1

let pow10 (n : int) : int =
  int_of_float @@ 10. ** float_of_int n

let take_digits (d : int) (x : int) : int =
  x / (pow10 @@ max 0 @@ digits x - d)

let cycle (n : int) (f : 'a -> 'a) : 'a -> 'a =
  let rec cycle' n x =
    if n = 0 then x else cycle' (n - 1) (f x)
  in
  cycle' n

let reduplicate (times : int) (n : int) : int =
  let exp = pow10 @@ digits n in
  cycle (times - 1) (fun x -> x * exp + n) n

let patterns_of (size : int) ((a, b) : Range.t) : int list =
  let max_prefix = take_digits size b in
  let chunks = (digits a) / (max 1 size) in
  let prefix = ref @@ take_digits size a in
  let invalids = ref [] in

  while !prefix <= max_prefix do
    let candidate = reduplicate chunks !prefix in
    if candidate >= a && candidate <= b then
      invalids := candidate :: !invalids;
    incr prefix;
  done;

  !invalids

module Part_1 = Solution(struct
  let invalids (a, b : Range.t) : int list =
    patterns_of (digits a / 2) (a, b)
end)

module Part_2 = Solution(struct
  let invalids (a, b : Range.t) : int list =
    let max_size = (digits b + 1) / 2 in
    List.init max_size (fun i -> patterns_of (i + 1) (a, b))
    |> List.flatten
end)
