open Types

val random_cell_for_krazy: state -> cell

val krazy_disappearing_sqs : state -> cell -> state

val krazy_cell_swap: state -> cell -> cell -> state

val krazy_switch_planes: state -> int -> int -> state

val krazy_bomb: state -> cell -> state

val do_krazy: command -> state -> state
