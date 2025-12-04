let year = 2025
let day = 4

open Import
open Grid

type map = [`R] GridMap.t

let parse : string -> map =
  grid_of @@ function
    | '@' -> Some `R
    | _ -> None

let show (map : map) : string =
  GridMap.fold (fun (x, y) _ res -> Printf.sprintf "%s\n%d, %d" res x y)
    map "\n"

let neighbours (x, y) : coord list =
  [
    x, y - 1;
    x, y + 1;
    x - 1, y;
    x - 1, y - 1;
    x - 1, y + 1;
    x + 1, y;
    x + 1, y - 1;
    x + 1, y + 1;
  ]

module Solution(Part : sig
  val solve : map -> int
end) : sig
  val run : string -> (string, string) result
end = struct
  let run = parse >> Part.solve >> string_of_int >> Result.ok
end

module Part_1 = Solution(struct
  let solve (map : map) : int =
    GridMap.fold (fun coord _ total ->
      let adjacent_rolls =
        neighbours coord
        |> List.filter (flip GridMap.mem map)
        |> List.length
      in
      total + if adjacent_rolls < 4 then 1 else 0
    ) map 0
end)

module Part_2 = Solution(struct
  let solve _ = failwith "todo"
end)
