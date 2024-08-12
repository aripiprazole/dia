type t = string Loc.t option

let make s = Loc.{ pos = { ending = 0; start = 0; file = "none" }; value = s }
