roxygen:
	cd .. && R -e "library(Rd2roxygen);options(width=60);rab('markdown')" && echo 'useDynLib(markdown, .registration = TRUE)' >> markdown/NAMESPACE
