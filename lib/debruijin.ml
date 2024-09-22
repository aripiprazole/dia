type lvl = int [@@deriving show]

type idx =
  | Idx of {
      value : int;
      name : string Loc.t;
    }
[@@deriving show]

(* Inverts the levels into indexes *)
let lvl_to_idx l x =
  Idx { name = Loc.{ pos = Loc.synthesized; value = "_" }; value = l - x - 1 }

(* Shifts level up *)
let shift l = l + 1
