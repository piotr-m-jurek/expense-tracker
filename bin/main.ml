open Expense_tracker
open Core
module CsvReader = Csv_utils.CsvOps (Csv_utils.ExpensesCsv)

let () =
  print_endline "";
  let expenses =
    CsvReader.read_csv ~filename:"data.csv"
    |> Domain.ExpensesStore.make
    |> Domain.ExpensesStore.add_expense ~description:"Flowers" ~amount:13.37
    |> Domain.ExpensesStore.add_expense ~description:"Coffee" ~amount:2.5
  in
  print_endline "";
  Domain.ExpensesStore.list expenses;
  print_endline "";
  Domain.ExpensesStore.summary expenses;
  print_endline "";
  print_endline "";
  CsvReader.write_csv ~filename:"data.bck.csv" expenses.list
;;

(* let () =
  let expenses =
    (* Read inital state from csv *)
    Domain.ExpensesStore.make ~list:[]
    |> Domain.ExpensesStore.add_expense ~description:"Coffee" ~amount:2.5
    |> Domain.ExpensesStore.add_expense ~description:"Cake" ~amount:5.
    |> Domain.ExpensesStore.add_expense ~description:"Flowers" ~amount:13.37
    |> Domain.ExpensesStore.add_expense ~description:"Dinner" ~amount:420.00
    |> Domain.ExpensesStore.remove_expense ~id:1
    |> Domain.ExpensesStore.update_expense ~id:2 ~description:"Tea" ~amount:3.5
  in
  print_endline "";
  Domain.ExpensesStore.list expenses;
  print_endline "";
  Domain.ExpensesStore.summary expenses;
  print_endline "";
  let raw_month = 1 in
  let _ =
    match Utils.Date.month_to_string ~raw_month with
    | Some month -> Domain.ExpensesStore.month_summary ~month expenses
    | None -> Printf.printf "Error parsing month: %d \n" raw_month
  in
  ()
;; *)
