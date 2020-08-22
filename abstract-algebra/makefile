pdfs:
	for i in *.tex; do latexmk -pdf "$i";done
	mkdir pdfs
	mv *.pdf pdfs
	rm -f *.aux *.toc *.log *.fdb_latexmk *.fls *.out
clean:
	rm -r ./pdfs/*
	rmdir pdfs
	rm -f *.aux *.toc *.log *.fdb_latexmk *.fls *.out
rmlogs:
	rm -f *.aux *.toc *.log *.fdb_latexmk *.fls *.out
rmpdfs:
	rm *.pdf
