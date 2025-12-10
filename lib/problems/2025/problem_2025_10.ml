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
  open Parser

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

let show : machine list -> string =
  let show_set = IntSet.elements >> List.map string_of_int >> String.concat "," in
  List.map (fun { lights; buttons; joltages } ->
    Printf.sprintf "Lights: %s\nButtons: %s\nJoltages: %s\n"
      (show_set lights)
      (buttons |> List.map show_set |> String.concat " ")
      (joltages |> List.map string_of_int |> String.concat ",")
  )
  >> String.concat "\n"

module Solution(Part : sig
  val solve : machine list -> int
end) = struct
  let run = Parse.parse >> Result.map show (*Part.solve >> string_of_int*)
end

module Part_1 = Solution(struct
  let solve _ = failwith "part 1"
end)

module Part_2 = Solution(struct
  let solve _ = failwith "part 2"
end)
