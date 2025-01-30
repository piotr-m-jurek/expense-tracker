open Expense_tracker

let () =
  let expenses =
    Domain.Expenses.make ()
    |> Domain.Expenses.add_expense ~description:"Coffee" ~amount:2.5
    |> Domain.Expenses.add_expense ~description:"Cake" ~amount:5.
    |> Domain.Expenses.add_expense ~description:"Flowers" ~amount:13.37
    |> Domain.Expenses.add_expense ~description:"Dinner" ~amount:420.00
    |> Domain.Expenses.remove_expense ~id:1
    |> Domain.Expenses.update_expense ~id:2 ~description:"Tea" ~amount:3.5
  in
  print_endline "";
  Domain.Expenses.list expenses;
  print_endline "";
  Domain.Expenses.summary expenses;
  print_endline "";
  let raw_month = 1 in
  let _ =
    match Utils.Date.month_to_string ~raw_month with
    | Some month -> Domain.Expenses.month_summary ~month expenses
    | None -> Printf.printf "Error parsing month: %d \n" raw_month
  in
  ()
;;
