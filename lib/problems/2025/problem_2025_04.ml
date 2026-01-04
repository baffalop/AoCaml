let year = 2025
let day = 4

open Base
open Import
open Grid

type map = [`Roll] GridMap.t

let parse : string -> map =
  grid_of @@ function
    | '@' -> Some `Roll
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

let accessible (coord : coord) ~map:(map : map) : bool =
  let adjacents = neighbours coord |> List.filter ~f:(Fn.flip GridMap.mem map) in
  List.length adjacents < 4

module Solution(Part : sig
  val solve : map -> int
end) : sig
  val run : string -> (string, string) Result.t
end = struct
  let run = parse >> Part.solve >> Int.to_string >> Result.return
end

module Part_1 = Solution(struct
  let solve (map : map) : int =
    GridMap.fold (fun coord _ total ->
      total + if accessible coord ~map then 1 else 0
    ) map 0
end)

module Part_2 = Solution(struct
  let remove_accessible (map : map) : map =
    GridMap.filter (fun coord _ -> not @@ accessible coord ~map) map

  let solve (map : map) : int =
    let rec reduce (map : map) : map =
      let reduced = remove_accessible map in
      if GridMap.equal Poly.(=) reduced map then map else reduce reduced
    in
    GridMap.cardinal map - GridMap.cardinal (reduce map)
end)
