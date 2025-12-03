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

module Part_1 = Solution(struct
  let find_max : jbank -> (int * int) =
    List.fold_left (fun (i, (res_i, res)) d ->
      (i + 1, if d > res then (i, d) else (res_i, res))
    ) (0, (0, 0))
    >> snd

  let max_joltage (bank : jbank) : int =
    let (i, fst_digit) = find_max @@ List.take (max 1 @@ List.length bank - 1) bank in
    let (_, snd_digit) = find_max @@ List.drop (i + 1) bank in
    fst_digit * 10 + snd_digit
end)

module Part_2 = Part_1
