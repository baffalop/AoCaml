open Import

type coord = int * int

module GridMap = Stdlib.Map.Make(struct
  type t = coord
  let compare = Poly.compare
end)

let grid_of (p : char -> 'a option) : string -> 'a GridMap.t =
  String.split ~on:'\n'
  >> List.fold ~init:(0, GridMap.empty) ~f:(fun (row, res) line ->
    row + 1,
    line
    |> String.fold ~init:(0, res) ~f:(fun (col, res) c ->
      col + 1,
      match p c with
      | Some v -> GridMap.add (col, row) v res
      | None -> res
    )
    |> snd
  )
  >> snd
