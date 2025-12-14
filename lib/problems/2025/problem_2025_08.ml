let year = 2025
let day = 8

open Import

type coord = int * int * int
type coords = coord list

let parse : string -> (coords, string) result =
  let open Angstrom in
  let open Parsers in
  parse_string ~consume:Prefix @@ lines_of @@
  let triple x y z = (x, y, z) in
  triple <$> u_dec <* char ',' <*> u_dec <* char ',' <*> u_dec

let show : coords -> string =
  List.map (fun (x, y, z) -> Printf.sprintf "(%d, %d, %d)" x y z)
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
