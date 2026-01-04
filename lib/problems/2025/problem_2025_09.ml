let year = 2025
let day = 9

open Base
open Import

type coord = int * int [@@deriving show]
type coords = coord list [@@deriving show]

let parse : string -> (coords, string) Result.t =
  let open Angstrom in
  let open Parsers in
  parse_string ~consume:Prefix @@ lines_of @@
  let* x = u_dec in
  let* y = char ',' *> u_dec in
  return (x, y)

module Solution(Part : sig
  val solve : coords -> int
end) : sig
  val run : string -> (string, string) Result.t
end = struct
  let run = parse >> Result.map ~f:show_coords (*Part.solve >> string_of_int*)
end

module Part_1 = Solution(struct
  let solve _ = failwith "part 1"
end)

module Part_2 = Solution(struct
  let solve _ = failwith "part 2"
end)
