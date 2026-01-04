let year = 2025
let day = 10

open Base
open Import

module IntSet = (val Set.make_showable (module Int) (Fmt.int))

type machine = {
  lights: IntSet.t;
  buttons: IntSet.t list;
  joltages: int list;
} [@@deriving show]

module Parse : sig
  val parse : string -> (machine list, string) Result.t
end = struct
  open Angstrom
  open Parsers

  let machine : machine t =
    let* lights_bool : bool list = bounded_by '[' ']' @@ many1 @@ choice [
      char '#' *> return true;
      char '.' *> return false;
    ] in
    let lights = lights_bool
      |> List.mapi ~f:(fun i x -> if x then Some i else None)
      |> List.filter_map ~f:id
      |> IntSet.of_list
    in
    char ' ' *>
    let* buttons : IntSet.t list = sep_by1 (char ' ')
      @@ bounded_by '(' ')' (IntSet.of_list <$> sep_by1 (char ',') u_dec)
    in
    char ' ' *>
    let* joltages = bounded_by '{' '}' @@ sep_by1 (char ',') u_dec in
    return { lights; buttons; joltages }

  let parse : string -> (machine list, string) Result.t =
    parse_string ~consume:Prefix @@ lines_of machine
end

let show_set = IntSet.elements >> List.map ~f:Int.to_string >> String.concat ~sep:","
let show_buttons = List.map ~f:show_set >> String.concat ~sep:" "
let show : machine list -> string =
  List.map ~f:(fun { lights; buttons; joltages } ->
    Printf.sprintf "Lights: %s\nButtons: %s\nJoltages: %s\n"
      (show_set lights)
      (show_buttons buttons)
      (joltages |> List.map ~f:Int.to_string |> String.concat ~sep:",")
  )
  >> String.concat ~sep:"\n"

module Solution(Part : sig
  val solve : machine list -> int
end) = struct
  let run = Parse.parse >> Result.map ~f:(Part.solve >> Int.to_string)
end

let rec choose (n : int) (pool : 'a list) : 'a list list =
  if n <= 0 then [[]]
  else pool
  |> flip List.take (List.length pool - n + 1)
  |> List.mapi ~f:(fun i x ->
    choose (n - 1) (List.drop pool (i + 1))
    |> List.map ~f:(List.cons x)
  )
  |> List.join

module Part_1 = Solution(struct
  let press_button (lights : IntSet.t) (button : IntSet.t) =
    IntSet.union lights button
    |> IntSet.filter (fun light ->
      Bool.(IntSet.mem light button <> IntSet.mem light lights)
    )

  let min_presses { lights; buttons; _ } =
    let rec try_choose (n : int) =
      let chosen = choose n buttons in
      let target_reached = chosen |> List.exists ~f:(fun buttons ->
          List.fold ~f:press_button ~init:IntSet.empty buttons
          |> IntSet.equal lights
      ) in
      if target_reached then n else try_choose (n + 1)
    in
    try_choose 1

  let solve : machine list -> int = List.map ~f:min_presses >> List.fold ~init:0 ~f:(+)
end)

module Part_2 = Solution(struct
  let solve _ = failwith "part 2"
end)
