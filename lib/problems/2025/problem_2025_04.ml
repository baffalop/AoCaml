let year = 2025
let day = 4

open Import

module GridMap = Parser.GridMap

type roll = R

type map = roll GridMap.t

module Parse : sig
  val parse : string -> (map, string) result
end = struct
  open Angstrom

  let map : (map t) = failwith "todo"

  let parse : string -> (map, string) result =
    parse_string ~consume:Prefix map
end

module Solution(Part : sig
  val solve : map -> int
end) : sig
  val run : string -> (string, string) result
end = struct
  let run = Parse.parse >> Result.map (Part.solve >> string_of_int)
end

module Part_1 = Solution(struct
  let solve = failwith "todo"
end)

module Part_2 = Solution(struct
  let solve = failwith "todo"
end)
