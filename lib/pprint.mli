val icit_to_pi_prefix : Syntax.icit -> string
val pprint : 'a -> string list -> Term.t -> string
val pp_pi_param : 'a -> string list -> Term.dom -> string
val pp_lam_param : Symbol.t * Syntax.icit -> string
