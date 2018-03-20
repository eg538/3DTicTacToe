open Yojson.Basic.Util

type command =
  |Play |Score |Quit |Restart|Try of string |Place of string|Hint |Look|Turns|Start

let parse str =
  let st = String.lowercase_ascii str in
  let space = if String.contains str ' ' then String.index st ' '
    else String.length str in
  let action = String.trim(String.sub st 0 space) in
  let obj = String.trim(String.sub st space ((String.length str) - space)) in
  match action with
  |"play" -> Play
  |"place" ->Place obj
  |"Try" -> Try obj
  |"start" -> Start
  |"quit"-> Quit
  |"look" -> Look
  |"score" ->Score
  |"turns" ->Turns
  |"restart" -> Restart
  |_ -> Place action
