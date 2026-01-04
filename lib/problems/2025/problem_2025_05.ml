let year = 2025
let day = 5

open Base
open Import

type range = int * int [@@deriving show]

module Inventory = struct
  type t = {
    ranges: range list;
    ingredients: int list;
  } [@@deriving show]
end

module Parse : sig
  val parse : string -> (Inventory.t, string) Result.t
end = struct
  open Angstrom

  let range : range t =
    let* a = Parsers.u_dec in
    char '-' *>
    let* b = Parsers.u_dec in
    return (a, b)

  let inventory : Inventory.t t =
    let* ranges = Parsers.lines_of range in
    string "\n\n" *>
    let* ingredients = Parsers.lines_of Parsers.u_dec in
    return Inventory.{ ranges; ingredients }

  let parse : string -> (Inventory.t, string) Result.t =
    parse_string ~consume:Prefix inventory
end

module Solution(Part : sig
  val solve : Inventory.t -> int
end) : sig
  val run : string -> (string, string) Result.t
end = struct
  let run = Parse.parse >> Result.map ~f:(Part.solve >> Int.to_string)
end

module Part_1 = Solution(struct
  open Inventory

  let within (id : int) ((a, b) : range) : bool = a <= id && id <= b

  let solve { ranges; ingredients } : int =
    ingredients
    |> List.filter ~f:(fun id -> List.exists ~f:(within id) ranges)
    |> List.length
end)

module Part_2 = Solution(struct
  let merge (ranges : range list) : range list =
    let sorted = ranges |> List.sort ~compare:(fun (a1, b1) (a2, b2) ->
      match compare a1 a2 with
      | 0 -> compare b1 b2
      | c -> c
    ) in
    let (results, final_range) =
      sorted
      |> Fn.flip List.drop 1
      |> List.fold ~init:([], List.hd_exn sorted)
        ~f:(fun (ranges, (prev_a, prev_b)) (a, b) ->
          if a <= prev_b then (ranges, (prev_a, max b prev_b))
          else ((prev_a, prev_b) :: ranges, (a, b))
        )
    in
    final_range :: results

  let solve Inventory.{ ranges } : int =
    merge ranges
    |> List.fold ~init:0 ~f:(fun total (a, b) -> total + (b - a + 1))
end)
