open Core

module Expense = struct
  type t =
    { id : int
    ; amount : float
    ; description : string
    ; date : string
    }

  let make ~id ~amount ~description ~date = { id; amount; description; date }

  let print =
    fun e -> Printf.sprintf "%-9d%-20s%-30s%10.2f" e.id e.date e.description e.amount
  ;;
end

module Expenses = struct
  type t =
    { list : Expense.t list
    ; current_id : int
    }

  let add_expense expenses ~description ~amount =
    let date = Utils.Date.(now () |> human_string) in
    let id = expenses.current_id in
    let expense = Expense.make ~id ~description ~amount ~date in
    { list = expense :: expenses.list; current_id = expenses.current_id + 1 }
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

  let make () = { list = []; current_id = 0 }

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

  let is_month_matching (e : Expense.t) ~(month : string) : bool =
    let expense_date = Utils.Date.make e.date in
    Utils.Date.month expense_date
    |> Option.value_map ~default:false ~f:(String.equal month)
  ;;

  let is_year_matching (e : Expense.t) ~(year : string) : bool =
    let expense_date = Utils.Date.make e.date in
    let expense_year = Utils.Date.year @@ expense_date in
    Option.value_map expense_year ~default:false ~f:(String.equal year)
  ;;

  let month_summary t ~month =
    let total =
      List.fold t.list ~init:0. ~f:(fun acc e ->
        let matching_month = is_month_matching e ~month in
        let current_year = Option.value_exn @@ Utils.Date.year @@ Utils.Date.now () in
        let matching_year = is_year_matching e ~year:current_year in
        if matching_month && matching_year then acc +. e.amount else acc)
    in
    Printf.printf "Total expenses for %s: %.2f\n" month total
  ;;
end
