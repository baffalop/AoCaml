open Core
open Let

type headers = (string * string) list

module Credentials: sig
  type t
  val of_auth_token : string -> t
  val to_headers : t -> headers
end = struct
  type t = string

  let of_auth_token (x : string) : t = x

  let to_headers (t : t) : headers =
    [ ("Cookie", "session=" ^ t) ]
end

module Run_mode = struct
  type t =
    | Example of { input : string option }
    | Test_from_puzzle_input of { credentials : Credentials.t option }
    | Submit of { credentials : Credentials.t }

  let read_file (filename : string) : string =
    In_channel.with_file filename ~f:(fun ch -> In_channel.input_all ch)

  let write_file (filename : string) (contents : string) : unit =
    Out_channel.write_all filename ~data:contents

  let init_cache (year : int) : string =
    if not (Stdlib.Sys.file_exists "inputs") then Stdlib.Sys.mkdir "inputs" 0o777;
    let year_dir = Filename.concat "inputs" @@ Int.to_string year in
    if not (Stdlib.Sys.file_exists year_dir) then Stdlib.Sys.mkdir year_dir 0o777;
    year_dir

  let get_example_input ~year:(year : int) ~day:(day : int) (input : string option) : (string, string) result =
    let year_dir = init_cache year in
    let filename = Filename.concat year_dir @@ Printf.sprintf "%02d-ex.txt" day in
    match input with
    | Some input -> (
      write_file filename input;
      Result.return input
    )
    | None ->
      if Stdlib.Sys.file_exists filename then Result.return @@ read_file filename
      else Error "No example input in cache: please pass in via stdin"

  let get_puzzle_input (year : int) (day : int)
      (credentials : Credentials.t option) : (string, string) result =
    (* Create cache directory structure *)
    let year_dir = init_cache year in
    (* Check if cached input exists *)
    let filename = Filename.concat year_dir @@ Printf.sprintf "%02d.txt" day in
    if Stdlib.Sys.file_exists filename then Result.return (read_file filename)
    else
      let () = print_endline "Input not cached: fetching from adventofcode.com..." in
      match credentials with
      | None ->
          Error "Cannot fetch input from adventofcode.com: missing credentials."
      | Some credentials ->
          Result.map_error ~f:(fun (code, msg) ->
              Printf.sprintf "[Code %d] %s" (Curl.int_of_curlCode code) msg)
          @@ Eio_main.run
          @@ fun env ->
          Eio.Switch.run
          @@ fun sw ->
          let url = Printf.sprintf "https://adventofcode.com/%d/day/%d/input" year day in
          let headers = Credentials.to_headers credentials in
          let@ { body } = Ezcurl.get ~url ~headers () in
          write_file filename body;
          Printf.printf "Got input; wrote to %s\n" filename;
          Result.return body

  let get_input (year : int) (day : int) : t -> (string, string) result =
    function
    | Example { input } -> get_example_input ~year ~day input
    | Test_from_puzzle_input { credentials } ->
        get_puzzle_input year day credentials
    | Submit { credentials } -> get_puzzle_input year day (Some credentials)

  let cleanup (year : int) (day : int) (part : int) (output : string)
      (run_mode : t) : (string option, string) result =
    match run_mode with
    | Test_from_puzzle_input _ | Example _ -> Result.return None
    | Submit { credentials } ->
        Result.map_error
          ~f:(fun (code, msg) ->
            Printf.sprintf "[Code %d] %s" (Curl.int_of_curlCode code) msg)
        @@ Eio_main.run
        @@ fun env ->
        Eio.Switch.run
        @@ fun sw ->
        let url = Printf.sprintf "https://adventofcode.com/%d/day/%d/answer" year day in
        let headers = Credentials.to_headers credentials
          @ [ ("Content-Type", "application/x-www-form-urlencoded") ]
        in
        let content = `String (Printf.sprintf "level=%d&answer=%s" part output) in
        let@ Ezcurl.{ body } = Ezcurl.post ~url ~headers ~content ~params:[] () in
        let html = Soup.parse body in
        let feedback =
          let open Soup in
          try html $ "main" |> Soup.R.leaf_text
          with
          | err -> Printf.sprintf "%s\n\n[Response received: parse error...\n%s]"
            body (Exn.to_string err)
        in
        Result.return @@ Some feedback
end

module Options = struct
  type t = { year : int; day : int; part : int; run_mode : Run_mode.t }
end

let run_problem (module Problem : Problem.T) (run_mode : Run_mode.t)
    ~(year : int) ~(day : int) ~(part : int) : (string, string) result =
  let@ input = Run_mode.get_input year day run_mode in
  let@ result =
    match part with
    | 1 -> Problem.Part_1.run input
    | 2 -> Problem.Part_2.run input
    | p -> Error (Printf.sprintf {|Invalid part "%d". Expected "1" or "2".|} p)
  in
  let@ cleanup_result = Run_mode.cleanup year day part result run_mode in
  let () =
    match cleanup_result with None -> () | Some result -> print_endline result
  in
  Result.return result

let find_problem (year : int) (day : int) : ((module Problem.T), string) result =
  match
    List.find
      ~f:(fun (module Problem : Problem.T) ->
        Problem.year = year && Problem.day = day)
      Problems.All.all
  with
  | Some p -> Result.return p
  | None ->
      Error
        (Printf.sprintf "Problem (year = %d, day = %d) not implemented."
           year day)

let run ({ year; day; part; run_mode } : Options.t) : (string, string) result =
  let@ problem = find_problem year day in
  run_problem problem run_mode ~year ~day ~part
