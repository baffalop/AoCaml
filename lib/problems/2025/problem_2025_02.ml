let year = 2025
let day = 2

open Import

module Range = struct
  type t = int * int

  let show (a, b) = Printf.sprintf "%d-%d (%d)" a b (b - a)

  let show_list : t list -> string = String.concat ", " << List.map show
end

module Parse : sig
  val parse : string -> (Range.t list, string) result
end = struct
  open Angstrom
  open Parser

  let range : Range.t t =
    let* a = u_dec <* char '-' in
    let* b = u_dec in
    return (a, b)

  let parse : string -> (Range.t list, string) result =
    parse_string ~consume:Prefix @@ sep_by1 (char ',') range
end

module Part_1 : sig
  val run : string -> (string, string) result
end = struct
  let run (input : string) : (string, string) result =
    let@ parsed = Parse.parse input in
    Ok (Range.show_list parsed)
end

module Part_2 : sig
  val run : string -> (string, string) result
end = struct
  let run (input : string) : (string, string) result = failwith "not implemented"
end
