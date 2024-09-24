open Symbol

type lvl = int [@@deriving show]
type idx = Idx of { value : int; name : Symbol.t } [@@deriving show]

(* Inverts the levels into indexes *)
let lvl_to_idx l x = Idx { name = Symbol.make K_hole "_"; value = l - x - 1 }

(* Shifts level up *)
let shift l = l + 1
