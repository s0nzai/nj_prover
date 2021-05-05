
module S = String
module L = List

type token =
    | TVar of string
    | TImpl
    | TAnd
    | TOr
    | TNot
    | TLParen
    | TRParen
    | TVDash
    | TComma
    | EOF

type lexbuf = {
    mutable buffer : string;
    mutable curr : int;
    mutable last : int;
}

exception Lexing of char

let buf = { buffer = ""; curr = 0; last = 0 }

let from_string s =
    buf.buffer <- s ^ " ";
    buf.curr <- 0;
    buf.last <- S.length s

let op_list = [("=>", TVDash); ("->", TImpl); ("|", TOr); ("&", TAnd); ("~", TNot);
    ("(", TLParen); (")", TRParen); (",", TComma)]

let is_space c =
    ((c = ' ') || (c = '\n') || (c = '\t'))

let is_alpha c =
    (('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z'))


let rec assoc x = function
    | [] -> None
    | ((k, y) :: _) when k = x -> Some(y)
    | (_ :: ys) -> assoc x ys

let variable () =
    let j = ref 0 in
    while is_alpha (buf.buffer.[buf.curr + !j]) do
        incr j
    done;
    let v = S.sub buf.buffer buf.curr !j in
    buf.curr <- buf.curr + !j;
    TVar(v)

let op_search len =
    let init = S.sub buf.buffer buf.curr len in
    assoc init op_list

let operator () =
    match op_search 2 with
    | Some(x) ->
            (buf.curr <- buf.curr + 2; x)
    | None ->
            (match op_search 1 with
            | Some(x) -> (buf.curr <- buf.curr + 1; x)
            | None -> raise (Lexing(buf.buffer.[buf.curr])))

let token () =
    while (buf.curr < buf.last) && is_space (buf.buffer.[buf.curr]) do
        buf.curr <- buf.curr + 1
    done;
    if buf.curr >= buf.last
    then EOF
    else if is_alpha (buf.buffer.[buf.curr])
    then variable ()
    else operator ()

let string_of_token = function
    | TVar(s) -> s
    | TImpl -> "->"
    | TAnd -> "&"
    | TOr -> "|"
    | TNot -> "~"
    | TLParen -> "("
    | TRParen -> ")"
    | TVDash -> "=>"
    | TComma -> ","
    | EOF -> ""

(*
let lex s =
    let buf = from_string s in
    let tok_list = ref [] in
    while buf.curr < !buf.last do
        let tok = token () in
        tok_list := tok :: !tok_list
    done;
    !tok_list
*)
