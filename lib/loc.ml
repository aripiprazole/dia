type t = {
  file : string;
  start : int;
  ending : int;
}
[@@deriving show]

let synthesized = { file = ""; start = 0; ending = 0 }
