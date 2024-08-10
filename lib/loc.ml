type pos = {
  file : string;
  start : int;
  ending : int;
}

type 't loc = {
  value : 't;
  pos : pos;
}

let synthesized = { file = ""; start = 0; ending = 0 }
