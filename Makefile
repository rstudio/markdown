roxygen:
	cd .. && R -e "library(Rd2roxygen);rab('markdown')" && echo 'useDynLib(markdown, .registration = TRUE)' >> markdown/NAMESPACE
