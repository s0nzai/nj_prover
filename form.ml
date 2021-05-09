
type t =
    | Var of string
    | Not of t
    | Impl of t * t
    | And of t * t
    | Or of t * t
    | Empty

let compare = Stdlib.compare

let rm_paren s =
    if s.[0] = '('
    then String.sub s 1 (String.length s - 2)
    else s

let rec string_of_ = function
    | Empty -> ""
    | Var(v) -> v
    | Not(p) -> "\\lnot " ^ (string_of_ p)
    | Impl(p, q) -> "(" ^ (string_of_ p) ^ "\\to " ^ (rm_paren (string_of_ q)) ^ ")"
    | And(p, q) -> "(" ^ (string_of_ p) ^ "\\land " ^ (string_of_ q) ^ ")"
    | Or(p, q) -> "(" ^ (string_of_ p) ^ "\\lor " ^ (string_of_ q) ^ ")"

let string_of p = rm_paren (string_of_ p)

let is_atomic = function
    | Var(_) -> true
    | _ -> false

