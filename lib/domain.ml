open Core

module Expense = struct
  type t =
    { id : int
    ; amount : float
    ; description : string
    ; date : Utils.Date.t
    }

  let make ~id ~amount ~description ~date = { id; amount; description; date }

  let raw_print e =
    Printf.sprintf
      "ID: %d\nDate: %s\nDescription: %s\nAmount: %.2f"
      e.id
      (Utils.Date.utc_string e.date)
      e.description
      e.amount
  ;;

  let print e =
    Printf.sprintf
      "%-9d%-20s%-30s%10.2f"
      e.id
      (Utils.Date.human_string e.date)
      e.description
      e.amount
  ;;
end

module ExpensesStore = struct
  type t =
    { header : string list
    ; list : Expense.t list
    ; current_id : int
    }

  type csv_file_contents =
    { header : string list
    ; data : string list list
    }

  let read_csv filename : csv_file_contents option =
    match filename |> In_channel.read_lines with
    | [] -> None
    | header :: data ->
      let header = String.split ~on:',' header in
      let data = List.map data ~f:(String.split ~on:',') in
      Some { header; data }
  ;;

  let write_csv filename { header; data } =
    let headers = String.concat ~sep:"," header in
    data
    |> List.map ~f:(fun row -> String.concat ~sep:"," row)
    |> fun lines -> Out_channel.write_lines filename (headers :: lines)
  ;;

  let add_expense expenses ~description ~amount =
    let date = Utils.Date.(now ()) in
    let id = expenses.current_id in
    let expense = Expense.make ~id ~description ~amount ~date in
    { expenses with
      list = expense :: expenses.list
    ; current_id = expenses.current_id + 1
    }
  ;;

  let update_expense epenses ~id ~description ~amount =
    let list =
      List.map
        ~f:(fun e -> if e.id = id then { e with description; amount } else e)
        epenses.list
    in
    { epenses with list }
  ;;

  let remove_expense t ~id =
    let list = List.filter ~f:(fun e -> e.id <> id) t.list in
    { t with list }
  ;;

  let make ~(list : Expense.t list) =
    let current_id =
      Option.map ~f:(fun v -> v.Expense.id + 1) @@ List.last list
      |> Option.value ~default:0
    in
    { list; current_id; header = [] }
  ;;

  let list t =
    Printf.printf "%-9s%-20s%-30s%10s\n" "ID" "Date" "Description" "Amount";
    Printf.printf "%s\n" (String.make 69 '-');
    t.list
    |> List.sort ~compare:(fun a b -> Int.compare a.Expense.id b.Expense.id)
    |> List.map ~f:Expense.print
    |> String.concat ~sep:"\n"
    |> Printf.printf "%s\n"
  ;;

  let summary t =
    let total = List.fold t.list ~init:0. ~f:(fun acc e -> acc +. e.amount) in
    Printf.printf "Total expenses: %.2f\n" total
  ;;

  let month_summary t ~month =
    let total =
      List.fold t.list ~init:0. ~f:(fun acc e ->
        let matching_month = Utils.Date.month_equals e.date ~month in
        let current_year = Option.value_exn @@ Utils.Date.get_year @@ Utils.Date.now () in
        let matching_year = Utils.Date.year_equals e.date ~year:current_year in
        if matching_month && matching_year then acc +. e.amount else acc)
    in
    Printf.printf "Total expenses for %s: %.2f\n" month total
  ;;
end
