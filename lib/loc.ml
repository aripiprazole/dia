type pos = {
  file : string;
  start : int;
  ending : int;
}
[@@deriving show]

type 'a t = {
  value : 'a;
  pos : pos;
}
[@@deriving show]

let synthesized = { file = ""; start = 0; ending = 0 }
let unwrap { value; pos = _ } = value
