# Vocabulink tests

all : web

web : web.lhs
	ghc -Wall -fglasgow-exts --make -o web $^

doc : web.pdf

web.pdf : web.tex
	pdflatex $<

web.tex : web.lhs
	lhs2TeX web.lhs > $@

test : web
	./web

clean :
	find . -iregex ".*\.\(o\|hi\|o-boot\|hi-boot\)" -print0 | xargs -0 rm
