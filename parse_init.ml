type level = Easy| Medium| Hard

type player = Caml | Python| None

type num_players = Single| Multi

type mode = Normal | Krazy

type info = {
  info_players_num : num_players;
  info_p1_avatar : player;
  info_level  : level;
  info_mode : mode
}

let rec parse_init str = 
  let lst = String.split_on_char ' ' (String.lowercase_ascii str) in
  {
  info_players_num = 
    if List.hd lst = "single" then
      Single 
    else
      Multi;
  info_p1_avatar = 
    if List.hd (List.tl lst) = "caml" then
      Caml
    else if List.hd (List.tl lst) = "python" then
      Python
    else
      None;
  info_mode = 
    if List.tl lst |> List.tl |> List.tl |> List.hd  = "normal" then
      Normal
    else
      Krazy;
  info_level = 
    match List.tl lst |> List.tl |> List.hd with
    | "easy" -> Easy
    | "medium" -> Medium
    | "hard" -> Hard
  }

let mode i = i.info_mode

let p1_avatar i = i.info_p1_avatar

let level i = i.info_level

let player_num i = i.info_players_num
