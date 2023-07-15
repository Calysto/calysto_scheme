scheme2023.pdf: scheme2023.tex scheme2023.bib acmart.cls ACM-Reference-Format.bst
	pdflatex scheme2023
	bibtex scheme2023
	pdflatex scheme2023
	pdflatex scheme2023

anon: scheme2023_anon.tex scheme2023_anon.bib acmart.cls ACM-Reference-Format.bst
	pdflatex scheme2023_anon
	bibtex scheme2023_anon
	pdflatex scheme2023_anon
	pdflatex scheme2023_anon

clean:
	rm -f *.pdf *.blg *.out *.log *.bbl *.aux *.cut *~
