open Effect.Deep

type t =
  | Flex of Term.meta_var * spine
  | Rigid of Debruijin.lvl * spine
  | Lam of Symbol.t * Core.icit * closure
  | Pi of Symbol.t * Core.icit * t * closure
  | U

and closure =
  | Closure of {
      env : env;
      expr : Term.t;
    }

and spine = (t * Core.icit) list
and env = t list

type hole =
  | Solved of t
  | Unsolved

(* Variable *)
let var x = Rigid (x, [])

(* Flexible variable *)
let meta x = Flex (x, [])

type _ Effect.t += Lookup_meta_var : Term.meta_var -> hole Effect.t
type _ Effect.t += Update_meta_var : Term.meta_var * t -> unit Effect.t
type _ Effect.t += Fresh_meta_var : unit -> Term.meta_var Effect.t

module Meta_env = Map.Make (Int)

let try_with f =
  let env : hole Meta_env.t ref = ref Meta_env.empty in
  let next_meta = ref (Term.Meta_var 0) in
  let handle_eff (type a) (eff : a Effect.t) =
    match eff with
    | Lookup_meta_var (Meta_var m) -> begin
        match Meta_env.find_opt m !env with
        | Some v -> Some (fun (k : (a, _) continuation) -> continue k v)
        | None -> None
      end
    | Update_meta_var (Meta_var m, v) ->
        Some
          (fun (k : (a, _) continuation) ->
            env := Meta_env.add m (Solved v) !env;
            continue k ())
    | Fresh_meta_var () ->
        Some
          (fun (k : (a, _) continuation) ->
            let (Term.Meta_var new_meta) = !next_meta in
            next_meta := Term.Meta_var (new_meta + 1);
            env := Meta_env.add new_meta Unsolved !env;
            continue k (Term.Meta_var new_meta))
    | _ -> None
  in
  try_with (f ()) { effc = handle_eff }
