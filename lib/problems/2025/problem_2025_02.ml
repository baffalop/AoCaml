let year = 2025
let day = 2

open Base
open Import

module Range = struct
  type t = int * int

  let show (a, b) = Printf.sprintf "%d-%d (%d)" a b (b - a)

  let show_list : t list -> string = String.concat ~sep:", " << List.map ~f:show
end

module Parse : sig
  val parse : string -> (Range.t list, string) Result.t
end = struct
  open Angstrom
  open Parsers

  let range : Range.t t =
    let* a = u_dec <* char '-' in
    let* b = u_dec in
    return (a, b)

  let parse : string -> (Range.t list, string) Result.t =
    parse_string ~consume:Prefix @@ lines_of range
end

module Solution(Part : sig
  val invalids : Range.t -> int list
end) : sig
  val run : string -> (string, string) Result.t
end = struct
  let run =
    Parse.parse
    >> Result.map ~f:(List.concat_map ~f:Part.invalids
      >> List.fold_left ~init:0 ~f:(+)
      >> Int.to_string)
end

let digits (n : int) : int =
  Float.to_int (Float.log10 @@ Float.of_int n) + 1

let take_digits (d : int) (x : int) : int =
  x / (10 ** (max 0 @@ digits x - d))

let cycle (n : int) (f : 'a -> 'a) : 'a -> 'a =
  let rec cycle' n x =
    if n = 0 then x else cycle' (n - 1) (f x)
  in
  cycle' n

let reduplicate (times : int) (n : int) : int =
  let exp = 10 ** digits n in
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
    Int.incr prefix;
  done;

  !invalids

module Part_1 = Solution(struct
  let invalids (a, b : Range.t) : int list =
    patterns_of (digits a / 2) (a, b)
end)

module Part_2 = Solution(struct
  let invalids (a, b : Range.t) : int list =
    let max_size = (digits b + 1) / 2 in
    List.init max_size ~f:(fun i -> patterns_of (i + 1) (a, b))
    |> List.join
end)
