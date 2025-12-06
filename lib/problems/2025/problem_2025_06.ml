let year = 2025
let day = 6

open Import

type op = Add | Mul

type sum = {
  inputs: int list;
  op: op;
}

type sums = sum list

let show_op = function
  | Add -> "+"
  | Mul -> "*"

let show_sum { inputs; op } : string =
  Fmt.str "%a %s" Fmt.(list ~sep:(any ", ") int) inputs (show_op op)

let show : sums -> string =
  List.map show_sum
  >> Fmt.str "%a" Fmt.(list ~sep:(any "\n") string)

module Parse : sig
  val parse : string -> (sums, string) result
end = struct
  open Angstrom
  open Parser

  let spaces : unit t = ignore <$> many (char ' ')
  let spaces1 : unit t = ignore <$> many1 (char ' ')

  let input_row : int list t = spaces *> sep_by1 spaces1 u_dec <* spaces

  let op_row : op list t =
    spaces *> sep_by1 spaces1 (choice [
      char '+' *> return Add;
      char '*' *> return Mul;
    ]) <* spaces

  let sums : sums t =
    let* input_rows = lines_of input_row in
    char '\n' *>
    let* ops = op_row in
    input_rows
    |> List.hd
    |> List.mapi (fun i _ -> {
      inputs = input_rows |> List.map (flip List.nth i);
      op = List.nth ops i
    })
    |> return

  let parse : string -> (sums, string) result = parse_string ~consume:Prefix sums
end

module Solution(Part : sig
  val solve : sums -> int
end) : sig
  val run : string -> (string, string) result
end = struct
  let run = Parse.parse >> Result.map (Part.solve >> string_of_int)
end

module Part_1 = Solution(struct
  let solve =
    List.fold_left (fun sum { inputs; op } ->
      let (op, id) = match op with
      | Add -> ( + ), 0
      | Mul -> ( * ), 1
      in
      sum + List.fold_left op id inputs
    ) 0
end)

module Part_2 = Solution(struct
  let solve _ = failwith "part 2"
end)
