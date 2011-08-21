# The Vocabulink Handbook

chapters:= $(shell ls chapters/*.tex)

all: handbook.pdf

handbook.pdf: handbook.ind
	xelatex handbook

handbook.ind: handbook.idx
	makeindex handbook

handbook.idx: handbook.tex handbook.sty $(chapters)
	xelatex handbook

clean:
	rm *.aux *.idx *.ilg *.ind *.log *.out *.toc
	rm chapters/*.aux