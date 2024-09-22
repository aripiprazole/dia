type t = string Loc.t option [@@deriving show]

let make s =
  Some Loc.{ pos = { ending = 0; start = 0; file = "none" }; value = s }
