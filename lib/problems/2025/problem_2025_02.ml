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

let digits (n : int) : int =
  int_of_float (log10 @@ float_of_int n) + 1

let pow10 (n : int) : int =
  int_of_float @@ 10. ** float_of_int n

let half_ceil (n : int) : int = (n + 1) / 2

let take_digits (d : int) (x : int) : int =
  x / (pow10 @@ max 0 @@ digits x - d)

let cycle (n : int) (f : 'a -> 'a) : 'a -> 'a =
  let rec cycle' n x =
    if n = 0 then x else cycle' (n - 1) (f x)
  in
  cycle' n

let reduplicate (times : int) (n : int) : int =
  let exp = pow10 @@ digits n in
  cycle (times - 1) (fun x -> x * exp + x) n

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
    let max_prefix = take_digits (half_ceil @@ digits b) b in
    let prefix = ref @@ take_digits (digits a / 2) a in
    let invalids = ref [] in

    Printf.printf "RANGE (%d, %d) max_prefix: %d\n" a b max_prefix;

    while !prefix <= max_prefix do
      let candidate = reduplicate 2 !prefix in
      if candidate >= a && candidate <= b then
        invalids := candidate :: !invalids;
      incr prefix;
    done;

    !invalids
end)

module Part_2 = Solution(struct
  let invalids (a, b : Range.t) : int list =
    let max_size = (digits a) / 2 in
    List.init (max_size - 1) (fun i -> (failwith "todo") (i + 1) (a, b))
    |> List.flatten
end)
