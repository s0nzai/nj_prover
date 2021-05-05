module C = Set.Make(Form)

let _ =
    let s = Sys.argv.(1) in
    (try Lexer.from_string s with
    | Lexer.Lexing(c) ->
            (prerr_string "Unexpected character: ";
            prerr_char c;
            prerr_newline ();
            exit 1));
    let (c, d) = try Parser.parse () with
        | Parser.Parse(t) ->
                (prerr_string "Parsing failure: ";
                prerr_string (Lexer.string_of_token t);
                prerr_newline ();
                exit 1) in
    let pt = Prove.prove (C.of_list c) d in
    let nat = Natural_tree.of_sequent pt in
    let sn = Natural_tree.string_of nat in
    print_string sn;
    print_newline ()

