let year = 2025
let day = 3

open Import

type jbank = int list

let show : jbank -> string = String.concat " " << List.map string_of_int

let parse : string -> jbank list =
  String.split_on_char '\n'
  >> List.map (String.to_seq >> List.of_seq
  >> List.map (Char.escaped >> int_of_string))

module Solution(Part : sig
  val max_joltage : jbank -> int
end) : sig
  val run : string -> (string, string) result
end = struct
  let run =
    parse
    >> List.map Part.max_joltage
    >> List.fold_left (+) 0
    >> string_of_int
    (* >> List.map string_of_int >> String.concat ", " *)
    >> Result.ok
end

let max_digit : jbank -> (int * int) =
  List.fold_left (fun (i, (res_i, res)) d ->
    (i + 1, if d > res then (i, d) else (res_i, res))
  ) (0, (0, 0))
  >> snd

let max_joltage_of (n : int) : jbank -> int =
  let rec max_joltage (jolts : int) (n : int) (bank : jbank) =
    if n = 0 then jolts else
    let next_n = n - 1 in
    let (i, joltage) = max_digit @@ List.take (max 1 @@ List.length bank - next_n) bank in
    max_joltage (jolts * 10 + joltage) next_n @@ List.drop (i + 1) bank
  in
  max_joltage 0 n

module Part_1 = Solution(struct
  let max_joltage = max_joltage_of 2
end)

module Part_2 = Solution(struct
  let max_joltage = max_joltage_of 12
end)
