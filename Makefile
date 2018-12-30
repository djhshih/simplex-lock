GHC=ghc -dynamic

all: simplex-lock
	

simplex-lock: simplex-lock.hs
	$(GHC) -o simplex-lock simplex-lock.hs

check: simplex-lock
	./simplex-lock > output.txt
	./check.sh

test: check

clean:
	rm -f *.{hi,o}
	rm -f simplex-lock output.txt

