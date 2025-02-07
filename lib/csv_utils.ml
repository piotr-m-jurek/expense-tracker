open Core

module type FileContent = sig
  type t

  val serialize : t -> string list
  val deserialize : string list -> t
end

module type CsvOpsType = sig
  type content

  val read_csv : filename:string -> content
  val write_csv : filename:string -> content -> unit
end

module CsvOps (Content : FileContent) : CsvOpsType with type content = Content.t = struct
  type content = Content.t

  let read_csv ~filename =
    Content.deserialize
    @@ Stdio.In_channel.with_file filename ~f:Stdio.In_channel.input_lines
  ;;

  let write_csv ~filename (data : content) =
    Content.serialize data |> Stdio.Out_channel.write_lines filename
  ;;
end

module ExpensesCsv = struct
  let header = [ "ID"; "AMOUNT"; "DESCRIPTION"; "DATE" ]

  type t = Domain.Expense.t list

  let serialize _ = []

  let deserialize (lines : string list) : t =
    match lines with
    | hd :: rest when List.equal String.equal header (String.split ~on:',' hd) ->
      List.fold rest ~init:[] ~f:(fun acc line ->
        match String.split ~on:',' line with
        | [ id; amount; description; date ] ->
          let sth =
            Domain.Expense.make
              ~id:(Int.of_string id)
              ~amount:(Float.of_string amount)
              ~description
              ~date
          in
          sth :: acc
        | r ->
          failwith
          @@ Printf.sprintf "error parsing one of the rows %s"
          @@ String.concat ~sep:"," r)
    | h :: _rest -> failwith @@ Printf.sprintf "header didn't match the schema: %s" h
    | _ -> failwith "lol nope"
  ;;
end
