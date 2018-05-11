open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson
open State

let lobby = ref []

let game_count = ref 0

let to_string = Yojson.Basic.Util.to_string
let to_list = Yojson.Basic.Util.to_list
let to_float = Yojson.Basic.Util.to_float
let member = Yojson.Basic.Util.member

type request =
  {
    body: string;
    params: (string * string) list
  }

let get_lobby () = lobby
