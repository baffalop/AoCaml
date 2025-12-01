let year = 2025
let day = 1

type turns = int list

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
  let (%) x y =
    let r = x mod y in
    if r < 0 then r + y else r

  let run (input : string) : (string, string) result =
    Parse.parse input |> Result.map (fun parsed ->
      List.fold_left (fun (state, zeroes) turn ->
        let state' = (state + turn) % 100 in
        ( state', zeroes + if state' = 0 then 1 else 0 )
      ) (50, 0) parsed
      |> snd
      |> string_of_int
    )
end

module Part_2 = struct
  let run (input : string) : (string, string) result = Error "Not implemented"
end
