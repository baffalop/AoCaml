let year = 2025
let day = 6

open Import

type op = Add | Mul
type padded_digits = int option list

module Presum = struct
  type t = {
    inputs: padded_digits list;
    op: op;
  }
end

module Sum = struct
  type t = {
    inputs: int list;
    op: op;
  }
end

type sums = Sum.t list

let show_op = function
  | Add -> "+"
  | Mul -> "*"

let show_sum Sum.{ inputs; op } : string =
  Fmt.str "%a %s" Fmt.(list ~sep:(any ", ") int) inputs (show_op op)

let show_padded_digits : padded_digits -> string =
  String.concat "" << List.map (function
    | None -> "."
    | Some d -> string_of_int d)

let show_presum Presum.{ inputs; op } : string =
  let inputs = String.concat " " @@ List.map show_padded_digits inputs in
  Printf.sprintf "%s %s" inputs (show_op op)

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
  val parse : string -> (Presum.t list, string) result
end = struct
  open Angstrom
  open Parser

  type op_segment = {
    op: op;
    length: int;
  }

  let spaces : int t = List.length <$> many (char ' ')
  let spaces1 : int t = List.length <$> many1 (char ' ')

  let padded_digits : padded_digits t =
    many1 @@ choice [
      return None <* char ' ';
      Option.some <$> digit;
    ]

  let op : op t = choice [
    char '+' *> return Add;
    char '*' *> return Mul;
  ]

  let op_row : op_segment list t =
    sep_by1 (char ' ') @@
    let* op = op in
    let* length = spaces in
    return { op; length }

  let presums : Presum.t list t =
    let* rows = lines_of padded_digits in
    char '\n' *>
    let* segments = op_row in
    segments
    |> List.fold_left_map (fun (inputs : padded_digits list) { op; length } ->
      inputs |> List.map (List.drop @@ length + 1),
      Presum.{
        op;
        inputs = inputs |> List.map (List.take length);
      }
    ) rows
    |> snd |> return

  let parse : string -> (Presum.t list, string) result =
    parse_string ~consume:Prefix presums
end

module Solution(Part : sig
  val make_sum : Presum.t -> Sum.t
end) : sig
  val run : string -> (string, string) result
end = struct
  let do_sums : sums -> int =
    List.fold_left (fun sum Sum.{ inputs; op } ->
      let (op, id) = match op with
      | Add -> ( + ), 0
      | Mul -> ( * ), 1
      in
      sum + List.fold_left op id inputs
    ) 0

  let run = Parse.parse >> Result.map (List.map show_presum >> String.concat "\n")
    (* List.map Part.make_sum >> do_sums >> string_of_int *)
end

module Part_1 = Solution(struct
  let make_sum Presum.{ inputs; op } : Sum.t = Sum.{
    inputs = List.map unpad inputs;
    op;
  }
end)

module Part_2 = Solution(struct
  let make_sum _ = failwith "part 2"
end)
