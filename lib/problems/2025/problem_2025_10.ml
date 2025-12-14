let year = 2025
let day = 10

open Import

module IntSet = Set.Make(Int)

type machine = {
  lights: IntSet.t;
  buttons: IntSet.t list;
  joltages: int list;
}

module Parse : sig
  val parse : string -> (machine list, string) result
end = struct
  open Angstrom
  open Parsers

  let machine : machine t =
    let* lights_bool : bool list = bounded_by '[' ']' @@ many1 @@ choice [
      char '#' *> return true;
      char '.' *> return false;
    ] in
    let lights = lights_bool
      |> List.mapi (fun i x -> if x then Some i else None)
      |> List.filter_map id
      |> IntSet.of_list
    in
    char ' ' *>
    let* buttons : IntSet.t list = sep_by1 (char ' ')
      @@ bounded_by '(' ')' (IntSet.of_list <$> sep_by1 (char ',') u_dec)
    in
    char ' ' *>
    let* joltages = bounded_by '{' '}' @@ sep_by1 (char ',') u_dec in
    return { lights; buttons; joltages }

  let parse : string -> (machine list, string) result =
    parse_string ~consume:Prefix @@ lines_of machine
end

let show_set = IntSet.elements >> List.map string_of_int >> String.concat ","
let show_buttons = List.map show_set >> String.concat " "
let show : machine list -> string =
  List.map (fun { lights; buttons; joltages } ->
    Printf.sprintf "Lights: %s\nButtons: %s\nJoltages: %s\n"
      (show_set lights)
      (show_buttons buttons)
      (joltages |> List.map string_of_int |> String.concat ",")
  )
  >> String.concat "\n"

module Solution(Part : sig
  val solve : machine list -> int
end) = struct
  let run = Parse.parse >> Result.map (Part.solve >> string_of_int)
end

let press_button (lights : IntSet.t) (button : IntSet.t) =
  IntSet.union lights button
  |> IntSet.filter (fun light ->
    IntSet.mem light button <> IntSet.mem light lights
  )

let rec choose (n : int) (pool : 'a list) : 'a list list =
  if n <= 0 then [[]] else
  pool
  |> List.take (List.length pool - n + 1)
  |> List.mapi (fun i x ->
    choose (n - 1) (List.drop (i + 1) pool)
    |> List.map (List.cons x)
  )
  |> List.flatten

let min_presses { lights; buttons; _ } =
  let rec try_choose (n : int) =
    let chosen = choose n buttons in
    let target_reached = chosen |> List.exists (fun buttons ->
        List.fold_left press_button IntSet.empty buttons
        |> IntSet.equal lights
    ) in
    if target_reached then n else try_choose (n + 1)
  in
  try_choose 1

module Part_1 = Solution(struct
  let solve : machine list -> int = List.map min_presses >> List.fold_left (+) 0
end)

module Part_2 = Solution(struct
  let solve _ = failwith "part 2"
end)
