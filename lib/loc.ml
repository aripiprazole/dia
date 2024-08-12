type pos = {
  file : string;
  start : int;
  ending : int;
}

type 'a t = {
  value : 'a;
  pos : pos;
}

let synthesized = { file = ""; start = 0; ending = 0 }
