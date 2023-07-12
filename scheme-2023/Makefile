scheme2023.pdf: scheme2023.tex scheme2023.bib acmart.cls ACM-Reference-Format.bst
	pdflatex scheme2023
	bibtex scheme2023
	pdflatex scheme2023
	pdflatex scheme2023

clean:
	rm -f *.pdf *.blg *.out *.log *.bbl *.aux *.cut *~
