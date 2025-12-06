let year = 2025
let day = 6

open Import

type op = Add | Mul
type padded_digits = int option list

type sum_grid = {
  inputs: padded_digits list list;
  ops: op list;
}

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

let rotate (matrix : 'a list list) : 'a list list =
  matrix
  |> List.hd
  |> List.mapi (fun i _ -> List.map (flip List.nth i) matrix)

let int_of_digits : int list -> int =
  List.fold_left (fun n d -> n * 10 + d) 0

let unpad : padded_digits -> int =
  List.filter_map id >> int_of_digits

module Parse : sig
  val parse : string -> (sum_grid, string) result
end = struct
  open Angstrom
  open Parser

  let spaces : unit t = ignore <$> many (char ' ')
  let spaces1 : unit t = ignore <$> many1 (char ' ')

  let padded_digits : padded_digits t =
    let* blanks = many @@ char ' ' *> return None in
    let* digits = many1 (Option.some <$> digit) in
    return @@ blanks @ digits

  let n_row : padded_digits list t =
    spaces *> sep_by1 (char ' ') padded_digits <* spaces

  let op_row : op list t =
    spaces *> sep_by1 spaces1 (choice [
      char '+' *> return Add;
      char '*' *> return Mul;
    ]) <* spaces

  let sum_grid : sum_grid t =
    let* inputs = lines_of n_row in
    char '\n' *>
    let* ops = op_row in
    return { inputs; ops }

  let parse : string -> (sum_grid, string) result = parse_string ~consume:Prefix sum_grid
end

module Solution(Part : sig
  val make_sums : sum_grid -> sums
end) : sig
  val run : string -> (string, string) result
end = struct
  let do_sums : sums -> int =
    List.fold_left (fun sum { inputs; op } ->
      let (op, id) = match op with
      | Add -> ( + ), 0
      | Mul -> ( * ), 1
      in
      sum + List.fold_left op id inputs
    ) 0

  let run = Parse.parse >> Result.map (Part.make_sums >> do_sums >> string_of_int)
end

module Part_1 = Solution(struct
  let make_sums { inputs; ops } : sums =
    rotate inputs
    |> List.mapi (fun i inputs -> {
      inputs = List.map unpad inputs;
      op = List.nth ops i
    })
end)

module Part_2 = Solution(struct
  let make_sums _ = failwith "part 2"
end)
