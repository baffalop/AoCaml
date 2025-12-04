open Import

type coord = int * int

module GridMap = Map.Make(struct
  type t = coord
  let compare = compare
end)

let grid_of (p : char -> 'a option) : string -> 'a GridMap.t =
  String.split_on_char '\n'
  >> List.fold_left (fun (row, res) line ->
    row + 1,
    line
    |> String.fold_left (fun (col, res) c ->
      col + 1,
      match p c with
      | Some v -> GridMap.add (col, row) v res
      | None -> res
    ) (0, res)
    |> snd
  ) (0, GridMap.empty)
  >> snd
