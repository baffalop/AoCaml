let year = 2025
let day = 5

open Import

type range = int * int

module Inventory = struct
  type t = {
    ranges: range list;
    ingredients: int list;
  }

  let show { ranges; ingredients } : string =
    Fmt.(str "Ranges:\n%a\n\nIngredients:\n%a\n"
      (list ~sep:(any "\n") @@ pair ~sep:(any ", ") int int) ranges
      (list ~sep:(any "\n") int) ingredients
    )
end

module Parse : sig
  val parse : string -> (Inventory.t, string) result
end = struct
  open Angstrom

  let range : range t =
    let* a = Parser.u_dec in
    char '-' *>
    let* b = Parser.u_dec in
    return (a, b)

  let inventory : Inventory.t t =
    let* ranges = Parser.lines_of range in
    string "\n\n" *>
    let* ingredients = Parser.lines_of Parser.u_dec in
    return Inventory.{ ranges; ingredients }

  let parse : string -> (Inventory.t, string) result =
    parse_string ~consume:Prefix inventory
end

module Solution(Part : sig
  val solve : Inventory.t -> int
end) : sig
  val run : string -> (string, string) result
end = struct
  let run = Parse.parse >> Result.map Inventory.show (*Part.solve >> string_of_int*)
end

module Part_1 = Solution(struct
  let solve _ = failwith "todo"
end)

module Part_2 = Solution(struct
  let solve _ = failwith "todo"
end)
