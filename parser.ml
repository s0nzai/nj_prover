
open Lexer

open Form

module L = List

exception Parse of token

let curr = ref EOF

let read_t () = curr := token ()

(*
let parse_error () =
    raise Parse(!curr)
*)

let bin_apply x y = function
    | TAnd -> And(x, y)
    | TOr -> Or(x, y)
    | _ -> raise (Parse(!curr))

(* left recursion for
 * right connection operators like E <- E op T | T *)
let right_op op_list term =
    let rec e_list () =
        let t = term () in
        if L.mem !curr op_list
        then
            (let op = !curr in
            read_t ();
            let e = e_list () in
            (op, t) :: e)
        else [] in
    match e_list () with
    | [] -> raise (Parse(!curr))
    | x :: xs ->
            let apply (op1, t1) (op2, t2) =
                (op2, bin_apply t1 t2 op1) in
            snd (L.fold_left apply x xs)

let rec prim () =
    let sym = !curr in
    read_t ();
    match sym with
    | TNot ->
            let e = prim () in
            Not(e)
    | TVar(v) -> Var(v)
    | TLParen ->
            let e = expr () in
            if !curr = TRParen
            then (read_t (); e)
            else raise (Parse(!curr))
    | _ -> raise (Parse(!curr))
and conj () =
    let e1 = prim () in
    if !curr = TOr
    then
        (read_t ();
        let e2 = conj () in
        Or(e1, e2))
    else e1
and disj () =
    let e1 = conj () in
    if !curr = TAnd
    then
        (read_t ();
        let e2 = disj () in
        And(e1, e2))
    else e1
and expr () =
    let e1 = disj () in
    if !curr = TImpl
    then
        (read_t ();
        let e2 = expr () in
        Impl(e1, e2))
    else e1

let rec expr_list () =
    if !curr = TVDash
    then []
    else
        let e = expr () in
        match !curr with
        | TVDash -> [e]
        | TComma ->
                (read_t ();
                e :: (expr_list ()))
        | _ -> raise (Parse(!curr))

let sequent () =
    let el = expr_list () in
    if !curr = TVDash
    then
        (read_t ();
        let e2 = expr() in
        (el, e2))
    else raise (Parse(!curr))

let parse () =
    read_t ();
    let s = sequent () in
    if !curr = EOF
    then s
    else raise (Parse(!curr))

(*
let _ =
    from_string "p -> q, q ->r |- r";
    let (c, d) = parse () in
    print_string (show_sequent c d);
    print_newline ()
*)

