let year = 2025
let day = 3

open Base
open Import

type jbank = int list [@@deriving show]

let parse : string -> jbank list =
  String.split ~on:'\n'
  >> List.map ~f:(String.to_list
  >> List.map ~f:(Char.escaped >> Int.of_string))

module Solution(Part : sig
  val max_joltage : jbank -> int
end) : sig
  val run : string -> (string, string) Result.t
end = struct
  let run =
    parse
    >> List.map ~f:Part.max_joltage
    >> List.fold ~init:0 ~f:(+)
    >> Int.to_string
    (* >> List.map ~f:string_of_int >> String.concat ~sep:", " *)
    >> Result.return
end

let max_digit : jbank -> (int * int) =
  List.fold ~init:(0, (0, 0))
    ~f:(fun (i, (res_i, res)) d ->
      (i + 1, if d > res then (i, d) else (res_i, res))
    )
  >> snd

let max_joltage_of (n : int) : jbank -> int =
  let rec max_joltage (jolts : int) (n : int) (bank : jbank) =
    if n = 0 then jolts else
    let next_n = n - 1 in
    let (i, joltage) = max_digit @@ List.take bank (max 1 @@ List.length bank - next_n) in
    max_joltage (jolts * 10 + joltage) next_n @@ List.drop bank (i + 1)
  in
  max_joltage 0 n

module Part_1 = Solution(struct
  let max_joltage = max_joltage_of 2
end)

module Part_2 = Solution(struct
  let max_joltage = max_joltage_of 12
end)
