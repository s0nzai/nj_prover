#!/bin/sh

echo "\\documentclass[10pt, a4paper]{article}
\\usepackage[a4paper, hmargin=21mm, vmargin=30mm]{geometry}
\\usepackage{graphicx}
\\usepackage{bussproofs}
\\def\\Ra{\\Rightarrow}
\\EnableBpAbbreviations
\\begin{document}
\\resizebox{\\textwidth}{!}{
\\noindent" > $2.tex
./nj_prover $1 >> $2.tex
echo "\\DisplayProof}
\\end{document}" >> $2.tex
pdflatex $2.tex

