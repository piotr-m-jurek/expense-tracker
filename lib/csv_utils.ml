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

  let serialize (t : t) : string list =
    let rows =
      t
      |> List.sort ~compare:(fun (e1 : Domain.Expense.t) (e2 : Domain.Expense.t) ->
        e1.id - e2.id)
      |> List.map ~f:(fun (expense : Domain.Expense.t) ->
        Printf.sprintf
          "%d,%.2f,%s,%s"
          expense.id
          expense.amount
          expense.description
          (Utils.Date.utc_string expense.date))
    in
    String.concat ~sep:"," header :: rows
  ;;

  let deserialize (lines : string list) : t =
    match lines with
    | hd :: rest when List.equal String.equal header (String.split ~on:',' hd) ->
      rest
      |> List.rev
      |> List.fold ~init:[] ~f:(fun acc line ->
        match String.split ~on:',' line with
        | [ id; amount; description; date ] ->
          let sth =
            Domain.Expense.make
              ~id:(Int.of_string id)
              ~amount:(Float.of_string amount)
              ~description
              ~date:(Utils.Date.of_string date)
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
