roxygen:
	cd .. && rm -f markdown_*.tar.gz && R -e "library(Rd2roxygen)" \
		-e "rab('markdown', build=FALSE)" && \
		echo "useDynLib(markdown, .registration = TRUE)" >> markdown/NAMESPACE && \
		R CMD build markdown && R CMD INSTALL markdown_*.tar.gz
