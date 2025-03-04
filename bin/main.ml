open Core
open Expense_tracker

let do_command (command : Domain.ExpensesStore.t -> Domain.ExpensesStore.t) =
  let module CsvReader = Csv_utils.CsvOps (Csv_utils.ExpensesCsv) in
  let filename = "data.csv" in
  let store = CsvReader.read_csv ~filename |> Domain.ExpensesStore.make |> command in
  CsvReader.write_csv ~filename store.list
;;

let list =
  Command.basic
    ~summary:"list all expenses"
    (Command.Param.return (fun () -> do_command Domain.ExpensesStore.list))
;;

let summary =
  Command.basic
    ~summary:"summary expenses from the month"
    (let%map_open.Command month =
       flag "--month" (optional string) ~doc:"Specify month number for summary"
     in
     fun () ->
       match month with
       | None -> do_command Domain.ExpensesStore.summary
       | Some month -> do_command @@ Domain.ExpensesStore.month_summary ~month)
;;

let add =
  Command.basic
    ~summary:"Add expense to the system"
    (let%map_open.Command description =
       flag "--description" (required string) ~doc:"short description of the expense"
     and amount = flag "--amount" (required float) ~doc:"expense amount" in
     fun () -> do_command @@ Domain.ExpensesStore.add_expense ~amount ~description)
;;

let remove =
  Command.basic
    ~summary:"Remove expense from the system"
    (* INFO: I'm leaving both combinatorial and preprocessed style of writing command line parsers, for educational sake *)
    Command.Param.(
      map
        (flag "--id" ~doc:"id of expense to delete" (required int))
        ~f:(fun id () -> do_command @@ Domain.ExpensesStore.remove_expense ~id))
;;

let () =
  Command.group
    ~summary:"Expense tracker app"
    [ "list", list; "summary", summary; "add", add; "remove", remove ]
  |> Command_unix.run
;;
