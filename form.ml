
type t =
    | Var of string
    | Not of t
    | Impl of t * t
    | And of t * t
    | Or of t * t
    | Empty

let compare = Stdlib.compare

let rec string_of_ = function
    | Empty -> ""
    | Var(v) -> v
    | Not(p) -> "\\lnot " ^ (string_of_ p)
    | Impl(p, q) -> "(" ^ (string_of_ p) ^ "\\to " ^ (string_of_ q) ^ ")"
    | And(p, q) -> "(" ^ (string_of_ p) ^ "\\land " ^ (string_of_ q) ^ ")"
    | Or(p, q) -> "(" ^ (string_of_ p) ^ "\\lor " ^ (string_of_ q) ^ ")"

let string_of p =
    let sp = string_of_ p in
    match p with
    | Var(_) | Not(_) -> sp
    | _ -> String.sub sp 1 (String.length sp - 2)

let is_atomic = function
    | Var(_) -> true
    | _ -> false

