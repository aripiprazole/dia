type lvl = int

type idx = {
  value : int;
  name : string Loc.t;
}

(* Inverts the levels into indexes *)
let lvl_to_idx l x =
  Loc.{ name = { pos = Loc.synthesized; value = "_" }; value = l - x - 1 }

(* Shifts level up *)
let shift l = l + 1
