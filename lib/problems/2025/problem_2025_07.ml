let year = 2025
let day = 7

open Base
open Import

module IntSet = (val Set.make_showable (module Int) (Fmt.int))

type manifold = {
  emitter: int;
  splitters: IntSet.t list;
} [@@deriving show]

let parse (input : string) : manifold =
  let lines = input |> String.split ~on:'\n' in
  let emitter = String.index_exn (List.hd_exn lines) 'S' in
  let splitters =
    List.drop lines 1
    |> List.map ~f:(String.fold ~init:(0, IntSet.empty)
      ~f:(fun (i, splitters) c ->
        i + 1, match c with
        | '^' -> IntSet.add i splitters
        | _ -> splitters
      )
    >> snd
    )
  in { emitter; splitters }

module Solution(Part : sig
  val solve : manifold -> int
end) : sig
  val run : string -> (string, string) Result.t
end = struct
  let run = parse >> Part.solve >> Int.to_string >> Result.return
end

module Part_1 = Solution(struct
  let solve { emitter; splitters } : int =
    splitters
    |> List.fold ~init:(0, IntSet.singleton emitter)
      ~f:(fun (total_splits, rays) splitter_row ->
        IntSet.fold (fun pos (total_splits, rays) ->
          if IntSet.mem pos splitter_row
          then (
            total_splits + 1,
            rays
            |> IntSet.add (pos + 1)
            |> IntSet.add (pos - 1)
          ) else (
            total_splits,
            rays |> IntSet.add pos
          )
        ) rays (total_splits, IntSet.empty)
      )
    |> fst
end)

module IntMap = Stdlib.Map.Make(Int)
type superposition = int IntMap.t

module Part_2 = Solution(struct
  let add_ray ~pos:(pos : int) ~count:(count : int) : superposition -> superposition =
    IntMap.update pos @@ function
      | None -> Some count
      | Some n -> Some (n + count)

  let solve { emitter; splitters } : int =
    let rays = splitters
      |> List.fold ~init:(IntMap.singleton emitter 1)
        ~f:(fun (rays : superposition) splitter_row ->
        IntMap.fold (fun pos count (rays : superposition) ->
          if IntSet.mem pos splitter_row
          then rays
            |> add_ray ~pos:(pos - 1) ~count
            |> add_ray ~pos:(pos + 1) ~count
          else rays |> add_ray ~pos ~count
        ) rays IntMap.empty
      )
    in
    IntMap.fold (fun _ count total -> total + count) rays 0
end)
