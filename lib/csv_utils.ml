open Core

let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)

module type CsvOps = sig
  type t

  val read_csv : string -> string list * string list list
  val write_csv : string -> string list -> string list list -> unit
end

module CsvOps : CsvOps = struct
  type t

  let read_csv filename =
    match filename |> In_channel.read_lines with
    | [] -> [], []
    | header :: data ->
      let header = String.split ~on:',' header in
      let data = List.map data ~f:(String.split ~on:',') in
      header, data
  ;;

  let write_csv filename headers data =
    let headers = String.concat ~sep:"," headers in
    data
    |> List.map ~f:(fun row -> String.concat ~sep:"," row)
    |> fun lines -> Out_channel.write_lines filename (headers :: lines)
  ;;
end

type ex = Domain.Expense.t

module CsvTypedParser = struct
  exception CSVParsingError of string

  let parse_int s =
    match Int.of_string_opt s with
    | Some i -> i
    | None ->
      let error = Printf.sprintf "Error parsing int '%s'" s in
      raise (CSVParsingError error)
  ;;

  let parse_float s =
    match Float.of_string_opt s with
    | Some f -> f
    | None ->
      let error = Printf.sprintf "Error parsing float '%s'" s in
      raise (CSVParsingError error)
  ;;

  let parse_bool s =
    match String.lowercase s with
    | "false" | "0" | "no" -> false
    | "true" | "1" | "yes" -> true
    | _ ->
      let error = Printf.sprintf "Error parsing bool '%s'" s in
      raise (CSVParsingError error)
  ;;

  let parse_string s =
    match String.strip s with
    | "" ->
      let error = Printf.sprintf "Error parsing bool '%s'" s in
      raise (CSVParsingError error)
    | s -> Some s
  ;;
end

module CsvProcessor (CsvOps : CsvOps) = struct
  type t = CsvOps.t

  let parse_typed_csv filename =
    let headers, data = CsvOps.read_csv filename in
    let parse_row row = row in
    headers, List.map data ~f:parse_row
  ;;

  let filter_rows ~predicate csv_data = List.filter csv_data ~f:predicate
  let map_rows ~f csv_data = List.map csv_data ~f

  let print_csv csv_data =
    let print_row = String.concat ~sep:"," >> Printf.printf "%s\n" in
    csv_data |> List.iter ~f:print_row
  ;;
end
