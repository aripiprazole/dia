open Loc

type t = string loc

let make s = { pos = { ending = 0; start = 0; file = "none" }; value = s }
