
all: main

clean:
	dune clean
	rm nj_prover

main:
	dune build
	cp _build/default/main.exe nj_prover
	chmod u+w nj_prover
