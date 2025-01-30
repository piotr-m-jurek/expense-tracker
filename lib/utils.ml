open Core

(* INFO: Very naive implementation of getting the month from UTC string*)
module Date : sig
  type t

  val make : string -> t
  val utc_string : t -> string
  val human_string : t -> string
  val now : unit -> t
  val month : t -> string option
  val year : t -> string option
  val pp : Out_channel.t -> t -> unit
  val month_to_string : raw_month:int -> string option
end = struct
  type t = UTCString of string

  (* TODO: add parsing *)
  let make s = UTCString s
  let utc_string (UTCString s) = s
  let human_string (UTCString s) = List.hd_exn @@ String.split ~on:' ' s
  let now () = Time_float.now () |> Time_float.to_string_utc |> make
  let pp fmt (UTCString s) = Printf.fprintf fmt "%s" s

  let month (UTCString s) =
    let list = s |> String.split ~on:'-' in
    List.nth list 1
  ;;

  let year (UTCString s) =
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

  (* parse string to get the year *)
end
