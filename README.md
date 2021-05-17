# nj\_prover
This is a toy program to put out a pdf contains a natural deduction proof diagrams of the intuitionistic first-order propositional logic.

## Usage

```Shell
./nj_prover deduction filename
```

_deduction_ is form of "p\_1,p\_1,...,p\_n => q", where p\_j and q are first-order propositional formulae.

The notation of a formula is same as the ordinary propositional formula, but using ASCII symbols, such that
- "->" for the implication
- "&" for the conjunction
- "|" for the disjunction
- "\~" for the negation

Precedence of the operators is "\~" > "&" > "|" > "-\>".

For example:
- "p-\>q-\>r,p-\>q=\>p-\>r"
- "=\>\~(p|q)-\>\~p&\~q"
- "=\>(p-\>q-\>r)-\>p&q-\>r"

_filename_ is the output file name without extensions.

## Requirements
- dune
- PDFLaTex
- LaTex packages: geometry, graphicx, bussproofs "p-\>q-\>r,p-\>r=\>"

