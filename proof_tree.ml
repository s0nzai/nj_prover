
open Printf
module F = Form
module C = Set.Make(Form)

type rule =
    | L_impl
    | R_impl
    | L_and
    | R_and
    | L_or
    | R_or
    | L_not
    | R_not
    | Ident

type t =
    | Node of rule * C.t * F.t * F.t * t * t
    | Nil

let string_of_seq _ pr d =
    (*let mc = List.map F.string_of (C.elements c)*)
    let sd = F.string_of d in
    if pr <> F.Empty
    then
        let spr = "[" ^ F.string_of pr ^ "]" in
        (*let sc = String.concat "," (mc @ [spr]) in*)
        spr ^ "\\Ra " ^ sd
    else
        (*let sc = String.concat "," mc in*)
        "\\Ra " ^ sd

let string_of_rule r =
    let rn = [(L_impl, "L\\to"); (R_impl, "R\\to");
    (L_and, "L\\land"); (R_and, "R\\land");
    (L_or, "L\\lor"); (R_or, "R\\lor");
    (L_not, "L\\lnot"); (R_not, "R\\lnot"); (Ident, "ID")] in
    List.assoc r rn

let rec string_of = function
    | Nil -> ""
    | Node(rule, c, pr, d, lt, rt) ->
            let sr = string_of_rule rule
            and sseq = string_of_seq c pr d
            and slt = string_of lt
            and srt = string_of rt in
            (match lt, rt with
            | Nil, Nil ->
                    sprintf "\\AXC{}\\LeftLabel{\\scriptsize $(%s)$}\n\\UIC{$%s$}" sr sseq
            | _, Nil ->
                    sprintf "%s\n\\LeftLabel{\\scriptsize $(%s)$}\n\\UIC{$%s$}" slt sr sseq
            | _, _ ->
                    sprintf "%s\n%s\\LeftLabel{\\scriptsize $(%s)$}\n\\BIC{$%s$}" slt srt sr sseq)

