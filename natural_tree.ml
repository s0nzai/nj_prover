
open Printf
module F = Form
module P = Lj_tree
module M = Map.Make(F)

let idx = ref 1
let get_idx () =
    let c = !idx in
    incr idx;
    c

type t =
    | Node of P.rule * int * F.t * t * t * t
    | Assume of int * F.t
    | Nil

let string_of_rule r =
    let rn = [(P.L_impl, "E\\to"); (P.R_impl, "I\\to");
    (P.L_and, "E\\land"); (P.R_and, "I\\land");
    (P.L_or, "E\\lor"); (P.R_or, "I\\lor");
    (P.L_not, "E\\lnot"); (P.R_not, "I\\lnot"); (P.Ident, "ID")] in
    List.assoc r rn

let rec string_of = function
    | Nil -> ""
    | Assume(k, d) ->
            let sd = F.string_of d in
            if k = 0
            then sprintf "\\AXC{$%s$}" sd
            else sprintf "\\AXC{}\\LeftLabel{\\scriptsize (%d)}\n\\UIC{$%s$}" k sd
    | Node(rule, k, d, lt, rt1, rt2) ->
            let sr = string_of_rule rule
            and sd = F.string_of d
            and slt = string_of lt
            and srt1 = string_of rt1
            and srt2 = string_of rt2 in
            (match lt, rt1, rt2 with
            | Nil, Nil, Nil ->
                    sprintf "\\AXC{}\\LeftLabel{\\scriptsize $(%s)$}\n\\UIC{$%s$}" sr sd
            | _, Nil, Nil ->
                    if k = 0
                    then sprintf "%s\n\\LeftLabel{\\scriptsize $(%s)$}\n\\UIC{$%s$}" slt sr sd
                    else sprintf "%s\n\\LeftLabel{\\scriptsize $(%s, %d)$}\\UIC{$%s$}" slt sr k sd
            | _, _, Nil ->
                    if k = 0
                    then sprintf "%s\n%s\\LeftLabel{\\scriptsize $(%s)$}\n\\BIC{$%s$}" slt srt1 sr sd
                    else sprintf "%s\n%s\\LeftLabel{\\scriptsize $(%s, %d)$}\n\\BIC{$%s$}" slt srt1 sr k sd
            | _, _, _ ->
                    sprintf "%s\n%s\n%s\\LeftLabel{\\scriptsize $(%s, %d)$}\n\\TIC{$%s$}" slt srt1 srt2 sr k sd)

let conseq = function
    | Nil -> F.Empty
    | Assume(_, d) -> d
    | Node(_, _, d, _, _, _) -> d

let rec graft t1 t2 = match t1, t2 with
    | _, Nil -> t2
    | _, Assume(_, d2) ->
            if conseq t1 = d2 then t1
            else t2
    | _, Node(rule, k, d2, lt, rt1, rt2) ->
            if conseq t1 = d2 then t1
            else
                let lt = graft t1 lt
                and rt1 = graft t1 rt1
                and rt2 = graft t1 rt2 in
                Node(rule, k, d2, lt, rt1, rt2)

let rec shorten_not = function
    | Nil -> Nil
    | Assume(_, _) -> Nil
    | Node(P.L_not, _, _, lt, rt, _) as t ->
            let lt = shorten_not lt
            and rt = shorten_not rt in
            (match lt, rt with
            | Nil, Nil -> t
            | Nil, rt -> rt
            | lt, _ -> lt)
    | Node(P.R_not, _, _, _, _, _) -> Nil
    | Node(P.R_impl, _, _, _, _, _) -> Nil
    | Node(P.L_or, _, _, _, _, _) -> Nil
    | Node(_, _, _, lt, rt, _) ->
            let lt = shorten_not lt
            and rt = shorten_not rt in
            (match lt, rt with
            | Nil, Nil -> Nil
            | Nil, rt -> rt
            | lt, _ -> lt)

let rec of_sequent_ m = function
    | P.Nil -> Nil
    | P.Node(P.Ident, _, _, d, _, _) ->
            let k = (try M.find d m with
            | Not_found -> 0) in
            Assume(k, d)
    | P.Node(P.L_or, _, F.Or(p, q), d, lt, rt) ->
            let k = (try M.find (Or(p, q)) m with
            | Not_found -> 0) in
            let t1 = Assume(k, Or(p, q)) in
            let j = get_idx () in
            let m = M.add q j (M.add p j m) in
            let lt = of_sequent_ m lt in
            let rt = of_sequent_ m rt in
            Node(P.L_or, j, d, t1, lt, rt)
    | P.Node(P.R_not, _, _, F.Not(p), lt, _) ->
            let (m, k) = let k = get_idx () in (M.add p k m, k) in
            let lt = of_sequent_ m lt in
            (match shorten_not lt with
            | Nil | Assume(_) ->
                    let t = Assume(k, p) in
                    Node(P.R_not, k, F.Not(p), lt, t, Nil)
            | Node(_, _, _, lt, rt, _) ->
                    Node(P.R_not, k, F.Not(p), lt, rt, Nil))
    | P.Node(P.R_impl, _, _, F.Impl(p, q), lt, _) ->
            let (m, k) = let k = get_idx () in (M.add p k m, k) in
            let lt = of_sequent_ m lt in
            (*let rt = of_sequent_ m rt in*)
            Node(P.R_impl, k, F.Impl(p, q), lt, Nil, Nil)
(*    | P.Node(P.L_false, _, F.False, d, lt, rt) ->
            let j = (try M.find F.False m with
                    | Not_found -> 0) in
            let t = Assume(j, F.False) in
            Node(P.L_false, 0, d, t, Nil, Nil)*)
    | P.Node(P.L_not, _, F.Not(p), d, lt, _) ->
            let lt = of_sequent_ m lt in
            let j = (try M.find (F.Not(p)) m with
            | Not_found -> 0) in
            (match shorten_not lt with
            | Nil | Assume(_) ->
                    let t1 = Assume(j, F.Not(p)) in
                    Node(P.L_not, 0, d, t1, lt, Nil)
            | Node(_, _, _, lt, rt, _) ->
                    Node(P.L_not, 0, d, lt, rt, Nil))
    | P.Node(P.L_impl, _, F.Impl(p, q), _, lt, rt) ->
            let lt = of_sequent_ m lt in
            let rt = of_sequent_ m rt in
            let j = (try M.find (F.Impl(p, q)) m with
                    | Not_found -> 0) in
            let t1 = Assume(j, F.Impl(p, q)) in
            let t2 = Node(P.L_impl, 0, q, t1, lt, Nil) in
            graft t2 rt
    | P.Node(P.L_and, _, F.And(p, q), _, lt, _) ->
            let lt = of_sequent_ m lt in
            let j = (try M.find (F.And(p, q)) m with
                    | Not_found -> 0) in
            let t1 = Assume(j, F.And(p, q)) in
            let t2 = Node(P.L_and, 0, q, t1, Nil, Nil) in
            let t3 = graft t2 lt in
            let t4 = Node(P.L_and, 0, p, t1, Nil, Nil) in
            graft t4 t3
    | P.Node(rule, _, _, d, lt, rt)  ->
            let lt = of_sequent_ m lt in
            let rt = of_sequent_ m rt in
            Node(rule, 0, d, lt, rt, Nil)

let of_sequent seq = of_sequent_ (M.empty) seq

