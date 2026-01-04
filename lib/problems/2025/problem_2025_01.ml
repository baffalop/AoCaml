let year = 2025
let day = 1

open Base
open Import

type turns = int list

module Parse : sig
  val parse : string -> (turns, string) Result.t
end = struct
  open Angstrom
  open Parsers

  let turn : int t =
    let* dir = choice [
      char 'L' *> return (-1);
      char 'R' *> return 1;
    ] in
    let* n = u_dec in
    return (dir * n)

  let parse : string -> (turns, string) Result.t =
    parse_string ~consume:Prefix @@ lines_of turn
end

module Solution(Part : sig
  val count_zeroes : state:int -> turn:int -> int
end) : sig
  val run : string -> (string, string) Result.t
end = struct
  let solve : turns -> int =
    List.fold_left ~init:(50, 0) ~f:(fun (state, zeroes) turn ->
      let state' = (state + turn) % 100 in
      let added_zeroes = Part.count_zeroes ~state ~turn in
      ( state', zeroes + added_zeroes )
    )
    >> snd

  let run = Parse.parse >> Result.map ~f:(solve >> Int.to_string)
end

module Part_1 = Solution(struct
  let count_zeroes ~state ~turn = if (state + turn) % 100 = 0 then 1 else 0
end)

module Part_2 = Solution(struct
  let count_zeroes ~state ~turn =
    let sum = state + turn in
    abs (sum / 100) + if sum <= 0 && state <> 0 then 1 else 0
end)
