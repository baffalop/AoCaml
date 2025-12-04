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

module Solution(Part : sig
  val solve : map -> int
end) : sig
  val run : string -> (string, string) result
end = struct
  let run = parse >> show (* Part.solve >> string_of_int *)
    >> Result.ok
end

module Part_1 = Solution(struct
  let solve _ = failwith "todo"
end)

module Part_2 = Solution(struct
  let solve _ = failwith "todo"
end)
