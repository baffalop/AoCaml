let year = 2025
let day = 9

open Import

type coord = int * int
type coords = coord list

let parse : string -> (coords, string) result =
  let open Angstrom in
  let open Parsers in
  parse_string ~consume:Prefix @@ lines_of @@
  let* x = u_dec in
  let* y = char ',' *> u_dec in
  return (x, y)

let show : coords -> string =
  List.map (fun (x, y) -> Printf.sprintf "(%d, %d)" x y)
  >> String.concat "\n"

module Solution(Part : sig
  val solve : coords -> int
end) : sig
  val run : string -> (string, string) result
end = struct
  let run = parse >> Result.map show (*Part.solve >> string_of_int*)
end

module Part_1 = Solution(struct
  let solve _ = failwith "part 1"
end)

module Part_2 = Solution(struct
  let solve _ = failwith "part 2"
end)
