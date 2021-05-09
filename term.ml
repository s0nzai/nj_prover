
open Printf

module F = Form
module N = Natural_tree
module L = Lj_tree
module C = Char

type t =
    | Var of int
    | Lambda of int * t
    | App of t * t
    | Pair of t * t
    | Proj of int * t
    | Inj of int * t
    | Case of int * t * t * t
    | Error of t * t

let rec construct = function
    | N.Nil -> Var(0)
    | N.Assume(j, _) -> Var(j)
    | N.Node(L.R_impl, j, _, lt, _, _) -> Lambda(j, construct lt)
    | N.Node(L.L_impl, _, _, lt, rt, _) -> App(construct lt, construct rt)
    | N.Node(L.R_and, _, _, lt, rt, _) -> Pair(construct lt, construct rt)
    | N.Node(L.L_and, _, d, lt, _, _) ->
            (match N.conseq lt with
            | And(_, q) when d = q -> Proj(2, construct lt)
            | And(_, _) -> Proj(1, construct lt)
            | _ -> Var(0))
    | N.Node(L.R_or, _, Or(_, q), lt, _, _) ->
            if q = N.conseq lt then Inj(2, construct lt)
            else Inj(1, construct lt)
    | N.Node(L.L_or, j, _, lt, lrt, rrt) ->
            Case(j, construct lrt, construct rrt, construct lt)
    | N.Node(L.R_not, j, _, lt, rt, _) ->
            Lambda(j, Error(construct lt, construct rt))
    | N.Node(L.L_not, _, _, lt, rt, _) -> Error(construct lt, construct rt)
    | _ -> Var(0)

let rm_paren s =
    if s.[0] = '('
    then String.sub s 1 (String.length s - 2)
    else s

let string_of_var j = C.escaped (C.chr (((j + 19) mod 26) + 97))

let rec string_of_ = function
    | Var(j) -> string_of_var j
    | Lambda(j, t) ->
            let s = rm_paren (string_of_ t) in
            sprintf "(\\lambda %s.%s)" (string_of_var j) s
    | App(t1, t2) ->
            let s1 = rm_paren (string_of_ t1)
            and s2 = string_of_ t2 in
            sprintf "(%s %s)" s1 s2
    | Pair(t1, t2) ->
            let s1 = rm_paren (string_of_ t1)
            and s2 = rm_paren (string_of_ t2) in
            sprintf "\\langle %s, %s\\rangle" s1 s2
    | Proj(j, t) ->
            let s = string_of_ t in
            sprintf "(\\pi_%d %s)" j s
    | Inj(j, t) ->
            let s = string_of_ t in
            sprintf "(\\iota_%d %s)" j s
    | Case(j, t1, t2, t3) ->
            let v = string_of_var j
            and s1 = rm_paren (string_of_ t1)
            and s2 = rm_paren (string_of_ t2)
            and s3 = string_of_ t3 in
            sprintf "([\\lambda %s.%s;\\lambda %s.%s]%s)" v s1 v s2 s3
    | Error(t1, t2) ->
            let s1 = rm_paren (string_of_ t1)
            and s2 = string_of_ t2 in
            sprintf "(!(%s %s))" s1 s2

let string_of t = rm_paren (string_of_ t)

