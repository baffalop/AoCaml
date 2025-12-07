let year = 2025
let day = 7

open Import

module IntSet = Set.Make(Int)

type manifold = {
  emitter: int;
  splitters: IntSet.t list;
}

let show { emitter; splitters } : string =
  Printf.sprintf "Emitter: %d\nSplitters:\n%s\n" emitter
    @@ String.concat "\n"
    @@ List.map
      (IntSet.elements >> List.map string_of_int >> String.concat ", ")
      splitters

module Parse : sig
  val parse : string -> manifold
end = struct
  let parse (input : string) : manifold =
    let lines = input |> String.split_on_char '\n' in
    let emitter = String.index (List.hd lines) 'S' in
    let splitters =
      List.drop 1 lines
      |> List.map (String.fold_left (fun (i, splitters) c ->
          i + 1, match c with
          | '^' -> IntSet.add i splitters
          | _ -> splitters
        ) (0, IntSet.empty)
        >> snd
      )
    in { emitter; splitters }
end

module Solution(Part : sig
  val solve : manifold -> int
end) : sig
  val run : string -> (string, string) result
end = struct
  let run = Parse.parse >> Part.solve >> string_of_int >> Result.ok
end

module Part_1 = Solution(struct
  let solve { emitter; splitters } : int =
    splitters
    |> List.fold_left (fun (total_splits, rays) splitters_row ->
      IntSet.fold (fun pos (total_splits, rays) ->
        if IntSet.mem pos splitters_row
        then (total_splits + 1, rays |> IntSet.add (pos + 1) |> IntSet.add (pos - 1))
        else (total_splits, rays |> IntSet.add pos)
      ) rays (total_splits, IntSet.empty)
    ) (0, IntSet.singleton emitter)
    |> fst
end)

module Part_2 = Solution(struct
  let solve _ : int = failwith "part 2"
end)
