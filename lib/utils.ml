open Core

(* INFO: Very naive implementation of getting the month from UTC string*)
module Date : sig
  type t

  val make : string -> t
  val utc_string : t -> string
  val human_string : t -> string
  val now : unit -> t
  val get_month : t -> string option
  val get_year : t -> string option
  val pp : Out_channel.t -> t -> unit
  val month_to_string : raw_month:int -> string option
  val month_equals : string -> month:string -> bool
  val year_equals : string -> year:string -> bool
end = struct
  type t = UTCString of string

  (* TODO: add parsing *)
  let make s = UTCString s
  let utc_string (UTCString s) = s
  let human_string (UTCString s) = List.hd_exn @@ String.split ~on:' ' s
  let now () = Time_float.now () |> Time_float.to_string_utc |> make
  let pp fmt (UTCString s) = Printf.fprintf fmt "%s" s

  let get_month (UTCString s) =
    let list = s |> String.split ~on:'-' in
    List.nth list 1
  ;;

  let get_year (UTCString s) =
    let list = s |> String.split ~on:'-' in
    List.hd list
  ;;

  let month_to_string ~raw_month =
    if raw_month > 0 && raw_month < 12
    then (
      let value = Int.to_string raw_month |> String.pad_left ~char:'0' ~len:2 in
      Some value)
    else None
  ;;

  let month_equals (date : string) ~(month : string) : bool =
    let expense_date = make date in
    get_month expense_date |> Option.value_map ~default:false ~f:(String.equal month)
  ;;

  let year_equals (date : string) ~(year : string) : bool =
    let expense_date = make date in
    let expense_year = get_year @@ expense_date in
    Option.value_map expense_year ~default:false ~f:(String.equal year)
  ;;

  (* parse string to get the year *)
end
