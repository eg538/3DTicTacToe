(* Runs the server on port number [port] *)
val run: int -> unit

val get_lobby: unit -> (int * Types.state) list ref
