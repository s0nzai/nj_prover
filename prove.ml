
open Form
open Proof_tree

module C = Set.Make(Form)

exception Unproved
exception Proved of Proof_tree.t

(* apply_r : Context.t -> Context.t -> Form.t -> Proof_tree.t
 * h : formulae already appeared
 * c : assumptions
 * d : consequent
 * apply the right rule to a sequent (h, c => d). *)
let rec apply_r h c = function
    | Not(p) as d when not (C.mem p c) ->
            let pc = C.add p c in
            let t = infer (C.singleton d) pc d in
            Node(R_not, c, Empty, d, t, Nil)
    | Impl(p, q) as d when not (C.mem p c) ->
            let pc = C.add p c in
            let t = infer (C.singleton q) pc q in
            Node(R_impl, c, Empty, d, t, Nil)
    | Impl(_, q) as d when not (C.mem q h) ->
            let t = infer (C.add q h) c q in
            Node(R_impl, c, Empty, d, t, Nil)
    | And(p, q) as d when not (C.mem p h) && not (C.mem q h) ->
            let lt = infer (C.add p h) c p
            and rt = infer (C.add q h) c q in
            Node(R_and, c, Empty, d, lt, rt)
    | Or(p, q) as d ->
            let t = (try
                if not (C.mem p h)
                then infer (C.add p h) c p
                else raise Unproved
            with Unproved ->
                if not (C.mem q h)
                then infer (C.add q h) c q
                else raise Unproved) in
            Node(R_or, c, Empty, d, t, Nil)
    | d -> focus h c d
(* apply_l : Context.t -> Context.t -> Form.t -> Form.t -> Proof_tree.t
 * h : formulae already appeared
 * c : assumptions
 * d : consequent
 * p : principal assumption
 * apply the left rule to a sequent (p, h, c => d) by which form of p. *)
and apply_l h c d = function
    | p when p = d -> Node(Ident, c, p, d, Nil, Nil)
    | Not(p) as r when not (C.mem p h) ->
            let t = infer (C.add p h) c p in
            Node(L_not, c, r, d, t, Nil)
    | Impl(p, q) as r when not (C.mem p h) ->
            let qc = C.add q c in
            let rt = apply_l h qc d q in
            let lt = infer (C.add p h) c p in
            Node(L_impl, c, r, d, lt, rt)
    | And(p, q) as r ->
            let pc = C.add p c
            and qc = C.add q c in
            let t = (try apply_l h pc d p
            with Unproved -> apply_l h qc d q) in
            Node(L_and, c, r, d, t, Nil)
    | Or(p, q) as r when not (C.mem p c) && not (C.mem q c) ->
            let pc = C.add p c
            and qc = C.add q c in
            let lt = infer (C.singleton d) pc d
            and rt = infer (C.singleton d) qc d in
            Node(L_or, c, r, d, lt, rt)
    | _ -> raise Unproved
and focus h c d =
    let f x = try
        (let t = apply_l h c d x in
        raise (Proved(t)))
            with Unproved -> () in
    try (C.iter f c; raise Unproved)
    with Proved(t) -> t
and infer h c = function
    | Or(_, _) | Var(_) as d ->
            (try focus h c d
            with Unproved -> apply_r h c d)
    | d -> apply_r h c d

let prove c d = infer C.empty c d

