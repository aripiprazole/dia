open Effect.Deep

type t =
  | V_flex of Term.meta_var * spine
  | V_rigid of Debruijin.lvl * spine
  | V_lam of Symbol.t * Concrete.icit * closure
  | V_pi of Symbol.t * Concrete.icit * t * closure
  | V_u
[@@deriving show]

and closure = Closure of { env : t list; expr : Term.t } [@@deriving show]
and spine = (t * Concrete.icit) list

type hole = Solved of t | Unsolved

(* Variable *)
let var x = V_rigid (x, [])

(* Flexible variable *)
let meta x = V_flex (x, [])

type _ Effect.t += Lookup_meta_var : Term.meta_var -> hole Effect.t
type _ Effect.t += Update_meta_var : Term.meta_var * t -> unit Effect.t
type _ Effect.t += Fresh_meta_var : unit -> Term.meta_var Effect.t

let ( <-- ) m v = Effect.perform @@ Update_meta_var (m, v)
let fresh () = Effect.perform @@ Fresh_meta_var ()
let lookup n = Effect.perform @@ Lookup_meta_var n

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
