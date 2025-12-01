let year = 2025
let day = 1

type turns = int list

let (%) x y =
  let r = x mod y in
  if r < 0 then r + y else r

let solve_with (count_zeroes : state:int -> turn:int -> int) (turns : turns) : int =
  List.fold_left (fun (state, zeroes) turn ->
    let state' = (state + turn) % 100 in
    let added_zeroes = count_zeroes ~state ~turn in
    ( state', zeroes + added_zeroes )
  ) (50, 0) turns
  |> snd

module Parse : sig
  val parse : string -> (turns, string) result
end = struct
  open Angstrom

  let u_dec : int t =
    let* digits = take_while1 (function '0'..'9' -> true | _ -> false) in
    return @@ int_of_string digits

  let turn : int t =
    let* dir = choice [
      char 'L' *> return (-1);
      char 'R' *> return 1;
    ] in
    let* n = u_dec in
    return (dir * n)

  let turns : turns t = sep_by1 (char '\n') turn

  let parse (input : string) : (turns, string) result = parse_string ~consume:Prefix turns input
end

module Print : sig
  val print : turns -> string
end = struct
  let print (turns : turns) : string = String.concat ", " @@ List.map string_of_int turns
end

module Part_1 : sig
  val run : string -> (string, string) result
end = struct
  let run (input : string) : (string, string) result =
    Parse.parse input
    |> Result.map (solve_with (fun ~state ~turn ->
      if (state + turn) % 100 = 0 then 1 else 0
    ))
    |> Result.map string_of_int
end

module Part_2 = struct
  let run (input : string) : (string, string) result =
    Parse.parse input
    |> Result.map (solve_with (fun ~state ~turn ->
      let accum = state + turn in
      abs (accum / 100) + if accum <= 0 && state <> 0 then 1 else 0
    ))
    |> Result.map string_of_int
end
