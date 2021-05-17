module C = Set.Make(Form)
module S = Sys
module P = Printf

let output_tex fn sn sterm =
    let header = "\\documentclass[10pt, a4paper]{article}\n"
        ^ "\\usepackage[a4paper, hmargin=21mm, vmargin=30mm]{geometry}\n"
        ^ "\\usepackage{graphicx}\n"
        ^ "\\usepackage{bussproofs}\n"
        ^ "\\def\\Ra{\\Rightarrow}\n"
        ^ "\\EnableBpAbbreviations\n"
        ^ "\\begin{document}\n"
        ^ "\\section{Proof tree}\n"
        ^ "\\noindent\n"
        ^ "\\resizebox{\\textwidth}{!}{\n"
    and end_of_tree = "\n\\DisplayProof}\n"
        ^ "\\section{Lambda Term}\n"
        ^ "\\[\n"
    and end_of_term = "\n\\]\n"
        ^ "\\end{document}\n" in
    let out = open_out (fn ^ ".tex") in
    output_string out header;
    output_string out sn;
    output_string out end_of_tree;
    output_string out sterm;
    output_string out end_of_term;
    close_out out

let _ =
    let s = Sys.argv.(1)
    and fn = Sys.argv.(2) in
    (try Lexer.from_string s with
        | Lexer.Lexing(c) ->
                (P.eprintf "Unexpected character: %c\n" c;
                exit 1));
    let (c, d) = try Parser.parse () with
        | Parser.Parse(t) ->
                (P.eprintf "Parsing failure: %s\n" (Lexer.string_of_token t);
                exit 1) in
    let pt = try Prove.prove (C.of_list c) d with
        | Prove.Unproved ->
                (prerr_string "Unproved\n";
                exit 1) in
    let nat = Natural_tree.of_sequent pt in
    let sn = Natural_tree.string_of nat in
    let term = Term.construct nat in
    let sterm = Term.string_of term in
    output_tex fn sn sterm;
    let _ = Sys.command ("pdflatex " ^ fn ^ ".tex") in
    ()

